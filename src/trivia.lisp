(in-package #:orca)

(defmodule trivia trivia-module ("trivia")
  (questions :accessor questions-of :initform nil
             :documentation "All the questions/answers available for asking, list of (QUESTION ANSWERS*)")
  (scores :accessor scores-of :initform (make-hash-table :test 'equal)
          :documentation "Count of correct answers of all the users who have played")
  (asked-questions :accessor asked-questions-of :initform nil
                   :documentation "Last asked question on a channel, list of (CHANNEL TIME QUESTION ANSWERS"))

(defmethod initialize-module ((module trivia-module) config)
  (load-trivia-data module))

(defun save-trivia-data (module)
  (with-open-file (ouf (orca-path "data/trivia-questions.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write (questions-of module) :stream ouf)
    (terpri ouf))
  (with-open-file (ouf (orca-path "data/trivia-scores.lisp"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create))
    (maphash (lambda (key val)
               (write (list key val) :stream ouf)
               (terpri))
             (scores-of module))))

(defun load-trivia-data (module)
  (with-open-file (inf (orca-path "data/trivia-questions.lisp") :direction :input)
    (setf (questions-of module) (read inf nil)))
  (with-open-file (inf (orca-path "data/trivia-scores.lisp") :direction :input)
    (clrhash (scores-of module))
    (loop for tuple = (read inf nil)
         while tuple
         do (setf (gethash (first tuple)
                        (scores-of module))
               (second tuple)))))

(defun channel-trivia-question (module channel)
  (assoc channel (asked-questions-of module) :test #'string-equal))

(defun channel-question-expired (module channel)
  (let ((current-q (channel-trivia-question module channel)))
    (and current-q (<= (- (get-universal-time) (second current-q)) 60))))

(defun deactivate-channel-question (module channel)
  (setf (asked-questions-of module)
        (delete (channel-trivia-question module channel)
                (asked-questions-of module))))

(defun ask-new-trivia-question (module channel)
  (deactivate-channel-question module channel)
  (let ((new-q (random-elt (questions-of module))))
    (push (list* channel (get-universal-time) new-q) (asked-questions-of module))
    (first new-q)))

(defun guess-trivia-answer (module channel user guess)
  (let ((current-q (assoc channel
                          (asked-questions-of module)
                          :test #'string=)))
    (when (find guess (cdddr current-q) :test #'string-equal)
      (incf (gethash user (scores-of module) 0))
      (deactivate-channel-question module channel)
      t)))

(defmethod handle-message ((module trivia-module)
                           (type (eql 'irc:irc-privmsg-message))
                           message)
  (guess-trivia-answer module (first (arguments message))
                       (source message)
                       (second (arguments message)))
  nil)

(defmethod handle-command ((module trivia-module)
                           (cmd (eql 'trivia))
                           message args)
  "trivia - ask a new trivia question"
  (cond
    ((not (message-target-is-channel-p message))
     (reply-to message "Keep the trivia in a channel, please"))
    ((null args)
     (let* ((channel (first (arguments message)))
            (current-q (channel-trivia-question module channel)))
       (cond
         ((channel-question-expired module channel)
          (when current-q
            (reply-to message "The answer was: ~a" (cadddr current-q)))
          (reply-to message "~a" (ask-new-trivia-question module channel)))
         (t
          (reply-to message "You have to allow at least one minute for answers.")))))

    ((string-equal (first args) "--help")
     (reply-to message "~trivia                    - ask a new trivia question"))))

;; TODO:
;; (reply-to message "~trivia --addq <text>      - add a new question")
;; (reply-to message "~trivia --adda <q#> <text> - add new answer to question")
;; (reply-to message "~trivia --remq <q#>        - remove a bad question")
;; (reply-to message "~trivia --rema <q#> <text> - remove bad answer to question")
;; (reply-to message "~trivia --top10            - top ten scores")
;; (reply-to message "~trivia --score [<nick>]   - score for one nick")