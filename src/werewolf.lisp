;;; Copyright 2012 Daniel Lowe All Rights Reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package #:orcabot)

;; Werewolf - a game in IRC
;;
;; .ww join         - join the werewolf game (only when unstarted)
;; .ww leave        - leave the werewolf game (any time)
;; .ww start        - start a new werewolf game with the current players
;; .ww list         - lists players. if a werewolf, indicates side
;; .ww vote <nick>  - votes on a player to eliminate. only werewolves can vote at night
;;
;; When a game is started, a certain amount of time is waited for
;; other players to join.  Once everyone has joined, the game starts.
;; Everyone is randomly assigned a werewolf or villager side.  The
;; game starts at night.
;;
;; At night, the werewolves decide by vote who to kill.  That person
;; is eliminated from the game.  Night ends once all the votes are
;; cast.
;;
;; At day, everyone decides by vote who to lynch.  That person is
;; eliminated from the game.
;;
;; The game continues until the werewolves equal the villagers
;; (werewolves win) or the villagers eliminate all the werewolves
;; (villagers win)

(defmodule werewolf werewolf-module ("ww")
  (state :accessor state-of :initform 'unstarted) ; unstarted, starting, night, or day
  (channel :accessor channel-of :initform nil) ; channel where the game has started
  (players :accessor players-of :initform nil) ; list of players
  (min-players :accessor min-players-of :initform 5)
  (max-players :accessor max-players-of :initform 20))

(defclass ww-player ()
  ((nick :accessor nick-of :initarg :nick :initform nil)
   (werewolf? :accessor werewolf? :initarg :werewolf? :initform nil)
   (alive? :accessor alive? :initarg :alive? :initform nil)
   (vote :accessor vote-of :initarg :vote :initform nil)))

(defmethod initialize-module ((module werewolf-module) config)
  (let ((module-conf (rest (assoc 'werewolf config))))
    (setf (channel-of module) (getf module-conf :channel "#werewolf"))
    (setf (min-players-of module) (getf module-conf :min-players 5))
    (setf (max-players-of module) (getf module-conf :max-players 20))))

(defmethod print-object ((object ww-player)
                         stream)
  (print-unreadable-object (object stream)
    (format stream "~w ~:[dead~;living~] ~:[villager~;werewolf~]"
            (nick-of object)
            (alive? object)
            (werewolf? object))))

(defun find-ww-player (module nick)
  (find nick (players-of module)
        :test #'string-equal
        :key 'nick-of))

(defun ww-inform (module player fmt &rest args)
  (cl-irc:privmsg (conn-of module) (nick-of player) (format nil "~?~%" fmt args)))

(defun ww-msg-players (module fmt &rest args)
  (cl-irc:privmsg (conn-of module) (channel-of module) (format nil "~?~%" fmt args)))

(defun ww-msg-wolves (module fmt &rest args)
  (dolist (player (players-of module))
    (when (and (alive? player)
               (werewolf? player))
      (apply 'ww-inform module player fmt args))))

(defun select-victim (voters)
  (let* ((votes (mapcar 'vote-of voters))
         (counts (mapcar (lambda (vote)
                           (cons vote (count vote votes)))
                         (remove-duplicates votes))))
    (car (first (sort counts #'> :key #'cdr)))))

(defun end-werewolf-game (module message)
  (ww-msg-players module message)
  (setf (state-of module) 'unstarted)
  (setf (players-of module) nil)
  (setf (channel-of module) nil))

(defun check-werewolf-win (module)
  (let ((werewolf-count (count-if (lambda (player)
                                    (and (alive? player)
                                         (werewolf? player)))
                                  (players-of module)))
        (villager-count (count-if (lambda (player)
                                    (and (alive? player)
                                         (not (werewolf? player))))
                                  (players-of module))))
    (cond
      ((>= werewolf-count villager-count)
       (end-werewolf-game module "The werewolves have won!  Nnargh!")
       t)
      ((zerop werewolf-count)
       (end-werewolf-game module "The villagers have won!  Yaay!")
       t)
      (t
       nil))))

(defun start-werewolf-nighttime (module)
  (assert (eql (state-of module) 'day))
  (setf (state-of module) 'night)
  (let ((victim (select-victim (players-of module))))
    (cond
      (victim
       (setf (alive? victim) nil)
       (ww-msg-players module "Evening comes...  The villagers come together and lynch ~a!" (nick-of victim))
       (check-werewolf-win module)
       (unless (eql (state-of module) 'unstarted)
         (ww-inform module victim
                    "You are now dead.  Feel free to watch, but please wait until the next game to participate.")))
      (t
       (ww-msg-players module "Evening comes...  The villagers decide to cower in their homes." ))))
  (unless (eql (state-of module) 'unstarted)
    (dolist (player (players-of module))
      (setf (vote-of player) nil))
    (ww-msg-players module "Nighttime has fallen...  The werewolves choose a victim in secret." )))

(defun start-werewolf-daytime (module)
  (assert (eql (state-of module) 'night))
  (setf (state-of module) 'day)
  (let ((victim (select-victim (remove-if-not 'werewolf? (players-of module)))))
    (cond
      (victim
       (setf (alive? victim) nil)
       (ww-msg-players module "Morning comes...  and ~a is found dead at home, ripped to shreds!"
                       (nick-of victim))
       (check-werewolf-win module)
       (unless (eql (state-of module) 'unstarted)
         (ww-inform module victim
                    "You are now dead.  Feel free to watch, but please wait until the next game to participate.")))
      (t
       (ww-msg-players module "Morning comes...  Everyone seems to be safe... Whew!" ))))
  (unless (eql (state-of module) 'unstarted)
    (dolist (player (players-of module))
      (setf (vote-of player) nil))
    (ww-msg-players module "The villagers come together in a town meeting to point fingers.")))

(defun list-werewolf-players (module player-nick)
  (let* ((player (find-ww-player module player-nick))
         (sorted-players (sort (players-of module) #'string< :key 'nick-of))
         (living (remove-if-not 'alive? sorted-players))
         (dead (remove-if 'alive? sorted-players)))
    (cond
      ((null player)
       (cl-irc:privmsg (conn-of module) player-nick "You are not playing the werewolf game."))
      ((werewolf? player)
       (ww-inform module player
                  "Werewolves: ~{~a~^, ~} / Food: ~{~a~^, ~} / Dead: ~:[Nobody yet~;~:*~{~a~^, ~}~]"
                  (mapcar 'nick-of (remove-if-not 'werewolf? living))
                  (mapcar 'nick-of (remove-if 'werewolf? living))
                  (mapcar 'nick-of dead)))

      (t
       (ww-inform module player
                  "The quick: ~{~a~^, ~} / The dead: ~:[Nobody yet~;~:*~{~a~^, ~}~]"
                  (mapcar 'nick-of living)
                  (mapcar 'nick-of dead))))))

(defun start-werewolf-game (module)
  (assert (eql (state-of module) 'unstarted))
  (setf (state-of module) 'night)
  ;; Pick werewolves
  (loop repeat (max 1 (floor (sqrt (length (players-of module))) 2)) do
       (let ((werewolf (random-elt (remove-if 'werewolf? (players-of module)))))
         (setf (werewolf? werewolf) t)))
  ;; Let everyone know their role
  (dolist (player (players-of module))
    (if (werewolf? player)
        (ww-inform module player "You are a werewolf.  You vote at night with other werewolves to murder villagers.   Be sure to /MSG ~a .ww vote <nick> to keep your identity private."
                   (nickname (user (conn-of module))))
        (ww-inform module player "You are a villager."))
    (list-werewolf-players module (nick-of player)))
  (ww-msg-players module "Nighttime has fallen...  The werewolves choose a victim in secret." ))

(defun add-werewolf-player (module nick)
  (assert (eql (state-of module) 'unstarted))
  (push (make-instance 'ww-player
                       :nick nick
                       :werewolf? nil
                       :alive? t)
        (players-of module)))

(defun remove-werewolf-player (module player)
  (setf (players-of module) (delete player (players-of module)))
  (cond
    ((not (eql (state-of module) 'unstarted))
     (ww-msg-players module "~a dies of a freak heart attack." (nick-of player))
     (setf (alive? player) nil))
    ((< (length (players-of module)) (min-players-of module))
     (ww-msg-players module "~a has left the werewolf game. (~a player~:p to go now)"
                     (nick-of player)
                     (- (min-players-of module) (length (players-of module)))))
    (t
     (ww-msg-players module "~a has left the werewolf game. Use .ww start to begin playing."
                     (nick-of player)))))

(defun tally-werewolf-vote (module player target)
  (cond
    ((not (alive? player))
     (ww-inform module player "You are dead and don't get a vote."))
    ((and (not (werewolf? player))
          (eql (state-of module) 'night))
     (ww-inform module player "Only the werewolves get to kill at night."))
    ((not (alive? target))
     (if (eql (state-of module) 'day)
         (ww-inform module player "You can't lynch the dead.")
         (ww-inform module player "Eat dead meat?  How disgusting!")))
    ((eql (state-of module) 'day)
     (setf (vote-of player) target)
     (ww-msg-players module "~a votes for lynching ~a." (nick-of player) (nick-of target))
     (when (every 'vote-of (remove-if-not 'alive? (players-of module)))
       (start-werewolf-nighttime module)))
    (t
     (setf (vote-of player) target)
     (ww-msg-wolves module "~a votes for eating ~a." (nick-of player) (nick-of target))
     (when (every 'vote-of (remove-if-not 'werewolf? (players-of module)))
       (start-werewolf-daytime module)))))

(defun ww-join-command (module message)
  (cond
    ((not (eql (state-of module) 'unstarted))
     (reply-to message "The werewolf game is already underway."))
    ((find-ww-player module (source message))
     (reply-to message "You've already entered the werewolf game."))
    ((>= (length (players-of module))
         (max-players-of module))
     (reply-to message "Sorry, the werewolf game is full right now.~%"))
    (t
     (add-werewolf-player module (source message))
     (if (< (length (players-of module))
            (min-players-of module))
         (ww-msg-players module "~a has joined the werewolf game. (~a player~:p to go)"
                         (source message)
                         (- (min-players-of module) (length (players-of module))))
         (ww-msg-players module "~a has joined the werewolf game. Use .ww start to being playing."
                         (source message))))))

(defun ww-leave-command (module message)
  (let ((player (find-ww-player module (source message))))
    (if player
        (remove-werewolf-player module player)
        (reply-to message "You're not in the werewolf game."))))

(defun ww-start-command (module message)
  (cond
    ((null (find-ww-player module (source message)))
     (reply-to message "Only a werewolf player may start the game."))
    ((not (eql (state-of module) 'unstarted))
     (reply-to message "The werewolf game is already underway."))
    ((< (length (players-of module)) (min-players-of module))
     (reply-to message "There ~[are no players~;is only 1 player~:;are only ~a players~] and you need ~a for a game."
               (length (players-of module))
               (min-players-of module)))
    (t
     (start-werewolf-game module))))

(defun ww-vote-command (module message target-nick)
  (let* ((player (find-ww-player module (source message)))
         (target (find-ww-player module target-nick)))
  (cond
    ((null player)
     (reply-to message "You are not playing the werewolf game."))
    ((null target)
     (reply-to message "They are not playing the werewolf game."))
    (t
     (tally-werewolf-vote module player target)))))

(defmethod handle-command ((module werewolf-module)
                           (cmd (eql 'ww))
                           message args)
  (alexandria:switch ((first args)
                       :test #'string-equal)
    ("join" (ww-join-command module message))
    ("leave" (ww-leave-command module message))
    ("start" (ww-start-command module message))
    ("list" (list-werewolf-players module (source message)))
    ("vote" (ww-vote-command module message (second args)))
    (t
     (reply-to message "Usage: .ww join|start|list|vote <nick>"))))