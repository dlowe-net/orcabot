(in-package #:orca)

(defmodule trivia trivia-module ("trivia")
  (questions :accessor questions-of :initform nil
             :documentation "All the questions/answers available for asking, list of (QUESTION ANSWERS*)")
  (scores :accessor scores-of :initform (make-hash-table :test 'equal)
          :documentation "Count of correct answers of all the users who have played")
  (asked-questions :accessor asked-questions-of :initform nil
                   :documentation "Last asked question on a channel, list of (CHANNEL TIME ID QUESTION"))

(defmethod initialize-module ((module trivia-module) config)
  (load-trivia-data module))

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

(defun save-trivia-questions (module)
  (with-open-file (ouf (orca-path "data/trivia-questions.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write (questions-of module) :stream ouf)
    (terpri ouf)))

(defun save-trivia-scores (module)
  (with-open-file (ouf (orca-path "data/trivia-scores.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (maphash (lambda (key val)
               (write (list key val) :stream ouf)
               (terpri))
             (scores-of module))))

(defun channel-trivia-question (module channel)
  (assoc channel (asked-questions-of module) :test #'string-equal))

(defun channel-question-expired (module channel)
  (let ((current-q (channel-trivia-question module channel)))
    (or (null current-q)
        (> (- (get-universal-time) (second current-q)) 60))))

(defun deactivate-channel-question (module channel)
  (setf (asked-questions-of module)
        (delete (channel-trivia-question module channel)
                (asked-questions-of module))))

(defun ask-new-trivia-question (module channel)
  (deactivate-channel-question module channel)
  (let* ((q-idx (random (length (questions-of module))))
         (new-q (nth q-idx (questions-of module))))
    (push (list channel (get-universal-time) (1+ q-idx) new-q) (asked-questions-of module))
    (format nil "~a. ~a" (1+ q-idx) (first new-q))))

(defun normalize-guess (guess)
  (string-trim
   '(#\space)
   (ppcre:regex-replace
    "^(the|a|an) "
    (ppcre:regex-replace-all
     "\\s+"
     (ppcre:regex-replace-all "[^\\s\\w]" (string-downcase guess) " ")
     " ")
    "")))

(defun guess-trivia-answer (module channel user guess)
  (let ((current-q (assoc channel
                          (asked-questions-of module)
                          :test #'string=))
        (normal-guess (normalize-guess guess)))
    (when (find normal-guess (rest (fourth current-q)) :test #'string=)
      (incf (gethash user (scores-of module) 0))
      (save-trivia-scores module)
      (deactivate-channel-question module channel)
      t)))

(defun add-trivia-question (module question)
  (let* ((split-q (ppcre:split "\\s*([.?])\\s*" question :with-registers-p t))
         (question-parts (loop
                            for (text punct) on split-q by #'cddr
                            collect (if punct
                                        (concatenate 'string text punct)
                                        text))))
    (setf (questions-of module)
          (nconc (questions-of module)
                 (list (list* (first question-parts)
                              (mapcar 'normalize-guess (rest question-parts)))))))
  (save-trivia-questions module)
  (length (questions-of module)))

(defun add-trivia-answer (module q-num answer)
  (let ((current-q (nth (1- q-num) (questions-of module))))
    (unless (member answer (cdddr current-q) :test #'string=)
      (setf (cdr (last current-q)) (list answer))
      (save-trivia-questions module))))

(defmethod handle-message ((module trivia-module)
                           (type (eql 'irc:irc-privmsg-message))
                           message)
  (when (guess-trivia-answer module (first (arguments message))
                             (source message)
                             (second (arguments message)))
    (reply-to message "Point goes to ~a (~a point~:p)"
              (source message)
              (gethash (source message) (scores-of module))))
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