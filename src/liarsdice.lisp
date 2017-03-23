(in-package #:orcabot)

(defmodule liarsdice liarsdice-module ("ld")
  (round :accessor round-of :initform nil)
  (current-player :accessor current-player-of :initform nil)
  (last-claim :accessor last-claim-of :initform nil
              :documentation "Last claim of form (NICK N D)")
  (players :accessor players-of :initform nil)
  (dice-left :accessor dice-left-of :initform nil)
  (dice-rolls :accessor dice-rolls-of :initform nil))

(defun liarsdice-setup-game (module message players)
  (cond
    ((players-of module)
     (reply-to message "There is already a game on!"))
    ((< (length players) 2)
     (reply-to message "You need at least two players!"))
    ((member (nickname (user (connection message)))
             players
             :test 'string-equal)
     (reply-to message "The bot can't play this game."))
    (t
     (setf (players-of module) (alexandria:shuffle players))
     (reply-to message "Starting game with player order: ~{~a~^, ~}" (players-of module))
     (setf (current-player-of module) (first (players-of module)))
     (setf (round-of module) 1
           (last-claim-of module) nil
           (dice-left-of module) (mapcar (lambda (nick)
                                           (cons nick 5))
                                         (players-of module)))
     (liarsdice-roll-dice module message))))

(defun liarsdice-roll-dice (module message)
  (reply-to message "Round #~a: Rolling ~a total dice.  It is ~a's turn."
            (round-of module)
            (reduce '+ (mapcar 'cdr (dice-left-of module)))
            (current-player-of module))
  (setf (dice-rolls-of module)
        (mapcar (lambda (nick)
                  (let ((rolls (sort (loop repeat (cdr (assoc nick (dice-left-of module) :test 'string-equal))
                                           collect (1+ (random 6)))
                                     '<)))
                    (irc:privmsg (connection message) nick
                                 (format nil "Round #~a: your dice rolls are ~{~a~^ ~}" (round-of module) rolls))
                    (cons nick rolls)))
                (remove-if (lambda (nick)
                             (zerop (cdr (assoc nick (dice-left-of module)
                                                :test 'string-equal))))
                           (players-of module)))))

(defun liarsdice-show-score (module message)
  (reply-to message "Dice left: ~:{~a - ~a~:^, ~}"
            (mapcar (lambda (x) (list (car x) (cdr x)))
                    (dice-left-of module))))

(defun liarsdice-penalize (module message nick)
  (let ((target (assoc nick (dice-left-of module) :test 'string-equal)))
    (decf (cdr target))
    
    (when (= 1 (count-if-not 'zerop (dice-left-of module) :key 'cdr))
      ;; one player remaining
      (reply-to message "~a loses a die and has been eliminated!" nick)
      (reply-to message "~a is the winner!"
                (car (find-if-not 'zerop (dice-left-of module) :key 'cdr)))
      (setf (players-of module) nil)
      (return-from liarsdice-penalize nil))
    
    (if (zerop (cdr target))
        (reply-to message "~a loses a die and has been eliminated!  It is now ~a's turn." nick)
        (reply-to message "~a loses a die!" nick))))

(defun advance-player (module)
  (loop
    (setf (current-player-of module)
          (elt (players-of module)
               (mod (1+ (position (current-player-of module)
                                  (players-of module)))
                    (length (players-of module)))))
    (when (plusp (cdr (assoc (current-player-of module)
                             (dice-left-of module)
                             :test 'string-equal)))
      (return-from advance-player))))

(defmethod handle-command ((module liarsdice-module)
                           (cmd (eql 'ld))
                           message args)
  ".ld [start <nicks>|claim|call|ragequit] - Facilitates a game of Liar's Dice."
  (unless (message-target-is-channel-p message)
    (reply-to message ".ld command should be used only in a channel.")
    (return-from handle-command nil))
  
  (alexandria:switch ((first args) :test 'string-equal)
    ("start"
     (liarsdice-setup-game module message (rest args)))
    ("claim"
     (let (n d)
       (cond
         ((endp (players-of module))
          (reply-to message "No game is in progress!"))
         ((string-not-equal (source message) (current-player-of module))
          (reply-to message "It's not your turn!"))
         ((or (/= 3 (length args))
              (null (setf n (parse-integer (second args) :junk-allowed t)))
              (null (setf d (parse-integer (third args) :junk-allowed t)))
              (minusp n)
              (not (<= 1 d 6)))
          (reply-to message "Usage: .ld claim <# of dice> <die roll>"))
         ((and (last-claim-of module)
               (or (< n (second (last-claim-of module)))
                   (and (= n (second (last-claim-of module)))
                        (< d (third (last-claim-of module))))))
          (reply-to message "# of dice needs to be greater than last claim, or die roll must be higher with same # of dice.")
          )
         (t
          (setf (last-claim-of module) (list (current-player-of module) n d))
          (advance-player module)
          (reply-to message "~a claims there ~:[are~;is~] ~a ~:[dice~;die~] showing ~a.  It is now ~a's turn."
                    (source message)
                    (= n 1)
                    n
                    (= n 1)
                    d
                    (current-player-of module))))
       ))
    ("call"
     (cond
       ((endp (players-of module))
        (reply-to message "No game is in progress!"))
       ((string-not-equal (source message) (current-player-of module))
        (reply-to message "It's not your turn!"))
       ((null (last-claim-of module))
        (reply-to message "No claim has been made yet!"))
       (t
        (reply-to message "--- round #~a results ---" (round-of module))
        (dolist (rolls (dice-rolls-of module))
          (reply-to message "~14a rolled ~{~a~^ ~}"
                    (first rolls)
                    (rest rolls)))
        (let ((roll-freq (make-array '(6) :initial-element 0))
              (n (second (last-claim-of module)))
              (d (third (last-claim-of module))))
          (dolist (rolls (dice-rolls-of module))
            (dolist (roll (rest rolls))
              (incf (aref roll-freq (1- roll)))))

          (reply-to message "There are ~:{~a ~as~:^, ~}"
                    (loop for freq across roll-freq
                          as idx from 1
                          unless (zerop freq)
                          collect (list freq idx)))
          (cond
            ((>= (aref roll-freq (1- d)) n)
             (reply-to message "~a's claim was at least ~a ~as, which was CORRECT." (first (last-claim-of module)) n d)
             (liarsdice-penalize module message (current-player-of module))
             (setf (current-player-of module) (first (last-claim-of module))))
            (t
             (reply-to message "~a's claim was at least ~a ~as, which was a LIE." (first (last-claim-of module)) n d)
             (liarsdice-penalize module message (first (last-claim-of module))))))
        (when (players-of module)
          ;; still playing?  Go to next round.
          (incf (round-of module))
          (liarsdice-show-score module message)
          (liarsdice-roll-dice module message)
          (setf (last-claim-of module) nil)))))
    
    ("ragequit"
     (cond
       ((endp (players-of module))
        (reply-to message "No game is in progress!"))
       ((not (member (source message)
                     (players-of module)
                     :test 'string-equal))
        (reply-to message "You're not one of the players, jerk."))
       (t
        (reply-to message "You flip the table over in a rage!  Dice go flying!")
        (setf (players-of module) nil))))
    (t
     (reply-to message "Usage: .ld [start <nicks>|claim|call|ragequit] - play a game of Liar's Dice."))))
