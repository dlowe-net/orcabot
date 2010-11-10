(in-package #:orca)

;;; Package for managing Magic: the Gathering tournaments
;;
;; Commands:
;;   mreset - resets the tournament to a new one
;;   madd   - adds a new member to the tournament
;;   mdrop  - drops a member from the tournament
;;   mmatch - add a match to the record, sets the title
;;   mscore - gives all scores
;;   mleft  - gives list of undone matches

(defvar *magic-players* nil)
(defvar *magic-matches* nil)

(defun magic-wins-of (nick)
  (count-if (lambda (match)
              (string-equal nick (car match)))
            *magic-matches*))

(defun magic-losses-of (nick)
  (count-if (lambda (match)
              (string-equal nick (cdr match)))
            *magic-matches*))

(defun find-match (winner loser)
  (find-if (lambda (match)
             (and (string-equal winner (cdr match))
                  (string-equal loser (car match))))
           *magic-matches*))

(defun make-score-card ()
  (flet ((compare-players (a b)
           (if (= (second a)
                  (second b))
               (string-lessp (first a) (first b))
               (> (second a)
                  (second b)))))
    (let ((players (mapcar (lambda (nick)
                             (list nick (magic-wins-of nick) (magic-losses-of nick)))
                           *magic-players*)))
      (format nil "~:{~a ~d-~d~:^, ~}"
              (sort players #'compare-players)))))

(defun unplayed-magic-matches ()
  (format nil "~:{~a-~a~:^, ~}"
          (loop
             for players on *magic-players*
             as player-a = (first players)
             append (loop for player-b in (rest players)
                       unless (or (string= player-a player-b)
                                  (find-match player-a player-b)
                                  (find-match player-b player-a))
                       collect (list player-a player-b)))))


(defun add-magic-players (nicks)
  (setf *magic-players* (sort (union *magic-players* nicks :test 'string-equal)
                              'string-lessp))
  (save-tournament))

(defun drop-magic-players (nicks)
  (setf *magic-players*
        (set-difference *magic-players* nicks :test 'string-equal))
  (save-tournament))

(defun reset-tournament ()
  (setf *magic-players* nil)
  (setf *magic-matches* nil))

(defun load-tournament ()
  (reset-tournament)
  (when (probe-file (orca-path "data/mtg.lisp"))
    (with-open-file (inf (orca-path "data/mtg.lisp")
                         :direction :input)
      (setf *magic-players* (read inf))
      (setf *magic-matches* (read inf)))))

(defun save-tournament ()
  (with-open-file (ouf (orca-path "data/mtg.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (print *magic-players* ouf)
    (print *magic-matches* ouf)))

(define-admin-command mreset (message directp)
  (reset-tournament))

(define-fun-command madd (message directp &rest nicks)
  (let ((old-player-len (length *magic-players*)))
    (add-magic-players nicks)
    (reply-to message "added ~d players" (- (length *magic-players*)
                                            old-player-len))))

(define-fun-command mdrop (message directp &rest nicks)
  (let ((old-player-len (length *magic-players*)))
    (drop-magic-players nicks)
    (reply-to message "dropped ~d players" (- old-player-len
                                              (length *magic-players*)))))

(define-fun-command mmatch (message directp winner loser)
  (cond
    ((null (find winner *magic-players* :test 'string-equal))
     (reply-to message "~a isn't playing" winner))
    ((null (find loser *magic-players* :test 'string-equal))
     (reply-to message "~a isn't playing" loser))
    ((string-equal winner loser)
     (reply-to message "Hah, hah.  Funny boy."))
    ((find-match winner loser)
     (reply-to message "result of match already entered."))
    ((find-match loser winner)
     (let ((match (find-match loser winner)))
       (setf (car match) winner)
       (setf (cdr match) loser)
       (save-tournament)
       (reply-to message "match altered.  now ~a is ~d-~d, ~a is ~d-~d"
                 winner
                 (magic-wins-of winner) (magic-losses-of winner)
                 loser
                 (magic-wins-of loser) (magic-losses-of loser))))
    (t
     (push (cons winner loser) *magic-matches*)
     (save-tournament)
     (reply-to message "match entered.  ~a is ~d-~d, ~a is ~d-~d"
               winner
               (magic-wins-of winner) (magic-losses-of winner)
               loser
               (magic-wins-of loser) (magic-losses-of loser)))))

(define-fun-command mscore (message directp)
  (reply-to message "~a" (make-score-card)))

(define-fun-command mleft (message directp)
  (reply-to message "~a" (unplayed-magic-matches)))
