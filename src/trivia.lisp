(in-package #:orca)

(defmodule trivia trivia-module ("trivia" "addtrivia" "deltrivia")
  (questions :accessor questions-of
             :documentation "All the questions/answers available for asking, adjustable vector of (QUESTION ANSWERS*)")
  (queue :accessor queue-of :documentation "A queue of the questions to be asked")
  (scores :accessor scores-of :initform (make-hash-table :test 'equal)
          :documentation "Count of correct answers of all the users who have played")
  (channel-questions :accessor channel-questions-of :initform nil
                   :documentation "Last asked question on a channel, list of (CHANNEL TIME ID QUESTION"))

(defmethod initialize-module ((module trivia-module) config)
  (load-trivia-data module)
  (populate-trivia-queue module))

(defun load-trivia-data (module)
  (with-open-file (inf (orca-path "data/trivia-questions.lisp") :direction :input)
    (let ((questions (read inf nil)))
      (setf (questions-of module)
            (make-array (length questions)
                        :initial-contents questions
                        :adjustable t
                        :fill-pointer t))))
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
    (write (coerce (questions-of module) 'list) :stream ouf)
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

(defun populate-trivia-queue (module)
  (let* ((count (length (questions-of module))))
    (setf (queue-of module) (make-random-list count count))))

(defun pop-trivia-question (module)
  (unless (queue-of module)
    (populate-trivia-queue module))
  (pop (queue-of module)))

(defun channel-trivia-question (module channel)
  (assoc channel (channel-questions-of module) :test #'string-equal))

(defun channel-question-expired (module channel)
  (let ((current-q (channel-trivia-question module channel)))
    (or (null current-q)
        (> (- (get-universal-time) (second current-q)) 60))))

(defun deactivate-channel-question (module channel)
  (setf (channel-questions-of module)
        (delete (channel-trivia-question module channel)
                (channel-questions-of module))))

(defun ask-new-trivia-question (module channel)
  (deactivate-channel-question module channel)

  (let* ((q-idx (pop-trivia-question module))
         (new-q (aref (questions-of module) q-idx)))
    (push (list channel (get-universal-time) (1+ q-idx) new-q)
          (channel-questions-of module))
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
                          (channel-questions-of module)
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
                                        text)))
         (new-question (list* (first question-parts)
                              (mapcar 'normalize-guess (rest question-parts)))))
    ;; insert at end of database
    (vector-push-extend new-question (questions-of module)))
  (save-trivia-questions module)
  (length (questions-of module)))

(defun delete-trivia-question (module q-num)
  (let* ((idx (1- q-num))
         (doomed-q (aref (questions-of module) idx)))
    (setf (questions-of module)
          (delete doomed-q (questions-of module)))
    (setf (queue-of module)
          (mapcar (lambda (i) (if (> i idx) (1- i) i))
                  (delete idx (queue-of module))))
    doomed-q))

(defun add-trivia-answer (module q-num answer)
  (let ((current-q (nth (1- q-num) (questions-of module))))
    (unless (member answer (cdddr current-q) :test #'string=)
      (setf (cdr (last current-q)) (list answer))
      (save-trivia-questions module))))

(defmethod handle-message ((module trivia-module)
                           (type (eql 'irc:irc-privmsg-message))
                           message)
  (when (and (message-target-is-channel-p message)
             (guess-trivia-answer module (first (arguments message))
                                  (source message)
                                  (second (arguments message))))
    (reply-to message "Point goes to ~a (~a point~:p)"
              (source message)
              (gethash (source message) (scores-of module))))
  nil)

(defmethod handle-command ((module trivia-module)
                           (cmd (eql 'trivia))
                           message args)
  "trivia - ask a new trivia question"
  (cond
    ((null args)
     (let* ((channel (first (arguments message)))
            (current-q (channel-trivia-question module channel)))
       (cond
         ((channel-question-expired module channel)
          (when current-q
            (reply-to message "The answer was: ~a" (second (fourth current-q))))
          (reply-to message "~a" (ask-new-trivia-question module channel)))
         (t
          (reply-to message "~a. ~a"
                    (third current-q)
                    (first (fourth current-q)))))))
    ((string-equal "--score" (first args))
     (let ((nick (or (second args)
                     (source message))))
       (reply-to message "~a has answered ~a trivia question~:p correctly."
                 nick (gethash nick (scores-of module) 0))))
    ((string-equal "--top" (first args))
     (let ((scores (sort (loop for nick being the hash-keys of (scores-of module)
                              as score = (gethash nick (scores-of module))
                              collect (list nick score))
                         #'> :key #'second)))
       (loop
          for tuple in scores
          for place from 1 upto 5
          do (reply-to message "~a. ~10a (~a)" place (first tuple)
                       (second tuple)))))
    (t
     (reply-to message "~~trivia                  - request a trivia question")
     (reply-to message "~~trivia --score [<nick>] - get score of user")
     (reply-to message "~~trivia --top            - list top trivia experts"))))

(defmethod handle-command ((module trivia-module)
                           (cmd (eql 'addtrivia))
                           message args)
  "addtrivia <question>[.?] <answer>. [<answer>.  ...] - add a new trivia question and answers"
  (cond
    ((null args)
     (reply-to message "Usage: ~~addtrivia <question>[.?] <answer>. [<answer>.  ...]"))
    (t
     (reply-to message "Question #~a created."
               (add-trivia-question module (join-string #\space args))))))

(defmethod handle-command ((module trivia-module)
                           (cmd (eql 'deltrivia))
                           message args)
  "deltrivia <question #> - delete a trivia question from the database"
  (if (null args)
      (reply-to message "Usage: ~~deltrivia <question #>")
      (let ((q-num (parse-integer (first args) :junk-allowed t)))
        (if (or (null q-num)
                (not (<= 1 q-num (length (questions-of module)))))
            (reply-to message "That's not a valid question number.")
            (progn
              (delete-trivia-question module (join-string #\space args))
              (reply-to message "Question #~a deleted." q-num))))))
