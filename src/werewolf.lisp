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
;; .ww start        - start a new werewolf game or join an existing one
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
  (players :accessor players-of :initform nil) ; list of ww-player
  (timer-of :accessor timer-of :initform nil)) ; timer used for next state

(defclass ww-player ()
  ((nick :accessor nick-of :initarg :nick :initform nil)
   (werewolf? :accessor werewolf? :initarg :werewolf? :initform nil)
   (alive? :accessor alive? :initarg :alive? :initform nil)
   (vote :accessor vote-of :initarg :vote :initform nil)))

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
  (declare (ignore module))
  (format t "~a: ~?~%" player fmt args)
  #+nil
  (cl-irc:privmsg (conn-of module) (nick-of player) (format nil "~?~%" fmt args)))


(defun ww-msg-players (module fmt &rest args)
    (format t "~a: ~?~%" (channel-of module) fmt args) #+nil
  (cl-irc:privmsg (conn-of module) (channel-of module) (format nil "~?~%" fmt args)))

(defun ww-msg-wolves (module fmt &rest args)
  (dolist (player (players-of module))
    (when (werewolf? player)
      (apply 'ww-inform module player fmt args))))

(defun select-victim (module)
  (let* ((votes (mapcar 'vote-of (players-of module)))
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
      ((= werewolf-count villager-count)
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
  (let ((victim (select-victim module)))
    (cond
      (victim
       (setf (alive? victim) nil)
       (ww-msg-players module "Evening comes...  The villagers come together and lynch ~a!")
       (check-werewolf-win module)
       (unless (eql (state-of module) 'unstarted)
         (cl-irc:privmsg (conn-of module) (nick-of victim)
                         "You are now dead.  Feel free to watch, but please wait until the next game to participate.")))
      (t
       (ww-msg-players module "Evening comes...  The villagers decide to cower in their homes." ))))
  (dolist (player (players-of module))
    (setf (vote-of player) nil))
  (ww-msg-players module "Nighttime has fallen...  The werewolves choose a victim in secret." ))

(defun start-werewolf-daytime (module)
  (assert (eql (state-of module) 'night))
  (setf (state-of module) 'day)
  (let ((victim (select-victim module)))
    (cond
      (victim
       (setf (alive? victim) nil)
       (ww-msg-players module "The day breaks...  ~a is found dead in their house, ripped to shreds!" )
       (check-werewolf-win module)
       (unless (eql (state-of module) 'unstarted)
         (cl-irc:privmsg (conn-of module) (nick-of victim)
                         "You are now dead.  Feel free to watch, but please wait until the next game to participate.")))
      (t
       (ww-msg-players module "The day breaks...  Everyone seems to be safe... Whew!" ))))
  (dolist (player (players-of module))
    (setf (vote-of player) nil)))

(defun list-werewolf-players (module player-nick)
  (let* ((player (find-ww-player module player-nick)))
    (cond
      ((null player)
       (cl-irc:privmsg (conn-of module) player-nick "You are not playing the werewolf game."))
      ((werewolf? player)
       (ww-inform module player
                  "Werewolves: ~{~a~^, ~} / Food: ~{~a~^, ~}"
                  (sort (mapcar 'nick-of (remove nil (players-of module) :key #'werewolf?))
                        #'string<)
                  (sort (mapcar 'nick-of (remove t (players-of module) :key #'werewolf?))
                        #'string<)))
      (t
       (ww-inform module player
                  "Players: ~{~a~^, ~}"
                  (sort (mapcar 'nick-of (players-of module)) #'string<))))))

(defun begin-werewolf-game (module)
  (assert (eql (state-of module) 'starting))
  (setf (state-of module) 'night)
  (loop repeat (max 1 (floor (sqrt (length (players-of module))) 2)) do
       (let ((werewolf (random-elt (remove-if 'werewolf? (players-of module)))))
         (setf (werewolf? werewolf) t)))
  (dolist (player (players-of module))
    (if (werewolf? player)
        (ww-inform module player "You are a werewolf.")
        (ww-inform module player "You are a villager."))
    (list-werewolf-players module (nick-of player)))
  (ww-msg-players module "Nighttime has fallen...  The werewolves choose a victim in secret." ))

(defun start-werewolf-game (module channel)
  (assert (eql (state-of module) 'unstarted))
  (setf (state-of module) 'starting)
  (setf (channel-of module) channel)
  #+nil
  (setf (timer-of module)
        (iolib:add-timer *event-base*
                         (lambda ()
                           (begin-werewolf-game module))
                         60 :one-shot t)))

(defun add-werewolf-player (module nick)
  (assert (eql (state-of module) 'starting))
  (push (make-instance 'ww-player
                       :nick nick
                       :werewolf? nil
                       :alive? t)
        (players-of module)))

(defun tally-werewolf-vote (module player target)
  (cond
    ((not (alive? player))
     (ww-inform module player "You are dead and don't get a vote."))
    ((and (not (werewolf? player))
          (eql (state-of module) 'day))
     (ww-inform module player "Only the werewolves get to kill at night."))
    ((not (alive? target))
     (if (eql (state-of module) 'day)
         (ww-inform module player "You can't lynch the dead.")
         (ww-inform module player "Eat dead meat?  How disgusting!")))
    ((eql (state-of module) 'day)
     (setf (vote-of player) (nick-of target))
     (ww-msg-players module "~a votes for lynching ~a." (nick-of player) (nick-of target))
     (when (every 'vote-of (players-of module))
       (start-werewolf-nighttime module)))
    (t
     (setf (vote-of player) (nick-of target))
     (ww-msg-wolves module "~a votes for eating ~a." (nick-of player) (nick-of target))
     (when (every 'vote-of (remove-if-not 'werewolf? (players-of module)))
       (start-werewolf-daytime module)))))

(defun ww-start-command (module message)
  (case (state-of module)
    (unstarted
     (cond
       ((message-target-is-channel-p message)
        (start-werewolf-game module (source message))
        (reply-to message "~a has started a new werewolf game.  Type .ww start to join in."))
       (t
        (reply-to message "You must start a new game on a channel."))))
    (started
     (add-werewolf-player module (source message))
     (ww-msg-players module "~a has joined the werewolf game." (source message)))
    (t
     (reply-to message "The werewolf game is already underway."))))

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
    ("start" (ww-start-command module message))
    ("list" (list-werewolf-players module (source message)))
    ("vote" (ww-vote-command module message (second args)))
    (t
     (reply-to message "Usage: .ww start|list|vote <nick>"))))

(defun test-ww ()
  (setf *m* (make-instance 'werewolf-module))
  (start-werewolf-game *m* "#ww")
  (add-werewolf-player *m* "Alice")
  (add-werewolf-player *m* "Bob")
  (add-werewolf-player *m* "Mallory")
  (begin-werewolf-game *m*))