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

(defmodule trivia trivia-module ("trivia" "addtrivia" "edittrivia" "deltrivia")
  (questions :accessor questions-of
             :documentation "All the questions/answers available for asking, adjustable vector of (ID QUESTION ANSWERS*)")
  (queue :accessor queue-of
         :documentation "A queue of the questions to be asked"
         :initform nil)
  (scores :accessor scores-of :initform nil
          :documentation "Alist of correct answers of all the users who have played, (USER ID ID ID...)")
  (channel-questions :accessor channel-questions-of :initform nil
                   :documentation "Last asked question on a channel, list of (CHANNEL TIME QUESTION"))

;;; Trivia questions
(defclass trivia-question ()
  ((id :accessor id-of :initarg :id)
   (text :accessor text-of :initarg :text)
   (answers :accessor answers-of :initarg :answers)))

(defmethod print-object ((object trivia-question)
                         stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~d. ~s" (id-of object)
            (text-of object))))

(defun deserialize-trivia-question (form)
  (if (numberp (first form))
      (make-instance 'trivia-question
                     :id (first form)
                     :text (second form)
                     :answers (cddr form))
      (make-instance 'trivia-question
                     :id nil
                     :text (first form)
                     :answers (cdr form))))

(defun serialize-trivia-question (question)
  `(,(id-of question)
     ,(text-of question)
     ,@(answers-of question)))

(defun load-trivia-questions (module)
  (with-open-file (inf (data-path "trivia-questions.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (cond
      (inf
       (let ((questions (read inf nil)))
         (setf (questions-of module)
               (make-array (length questions)
                           :adjustable t
                           :fill-pointer t))
         (map-into (questions-of module) 'deserialize-trivia-question questions)
         (loop
            for question across (questions-of module)
            as idx from 1
            when (null (id-of question))
            do (setf (id-of question) idx))))
      (t
       (setf (questions-of module) (make-array 0 :adjustable t :fill-pointer t))))))

(defun save-trivia-questions (module)
  (with-open-file (ouf (data-path "trivia-questions.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)

    (write (map 'list 'serialize-trivia-question (questions-of module))
           :stream ouf)
    (terpri ouf)))

(defun string-to-question-answers (str)
  (let* ((split-q (nth-value 1 (ppcre:scan-to-strings "\\s*(.*?[?.])\\s*(.+)" str)))
         (answers (and split-q
                       (delete "" (ppcre:split "\\s*\\.\\s*" (elt split-q 1))
                               :test #'string=))))
    (when answers
      (values (elt split-q 0) answers))))

(defun trivia-questions-empty-p (module)
  (zerop (length (questions-of module))))

(defun retrieve-trivia-question (module num)
  (find num (questions-of module)
        :key 'id-of))

(defun add-trivia-question (module spec)
  "Given a string SPEC, adds a new trivia question to the trivia
module.  Returns the ID of the question if successful, otherwise
return NIL."
  (multiple-value-bind (question-text answers)
      (string-to-question-answers spec)
    (when question-text
      (let* ((new-id (1+ (reduce #'max (map 'vector 'id-of (questions-of module)))))
             (new-question (make-instance 'trivia-question
                                          :id new-id
                                          :text question-text
                                          :answers answers)))
        ;; insert at end of database
        (vector-push-extend new-question (questions-of module))
        (save-trivia-questions module)
        new-id))))

(defun edit-trivia-question (module q-num spec)
  (multiple-value-bind (question-text new-answers)
      (string-to-question-answers spec)
    (when question-text
      (with-slots (text answers) (retrieve-trivia-question module q-num)
        (setf text question-text
              answers new-answers)
        (save-trivia-questions module)
        nil))))

(defun delete-trivia-question (module q-num)
  (let* ((doomed-q (find q-num (questions-of module) :key 'id-of)))
    (setf (questions-of module)
          (delete doomed-q (questions-of module)))
    (setf (queue-of module)
          (delete doomed-q (queue-of module)))
    (save-trivia-questions module)
    doomed-q))

(defun add-trivia-answer (module q-num answer)
  (let ((current-q (nth (1- q-num) (questions-of module))))
    (unless (member answer (answers-of current-q) :test #'string=)
      (setf (answers-of current-q) (append (answers-of current-q)
                                           (list answer)))
      (save-trivia-questions module))))

(defun normalize-guess (guess)
  (string-downcase
   (string-trim
    '(#\space)
    (ppcre:regex-replace
     "^(the|a|an) "
     (ppcre:regex-replace-all
      "\\s+"
      (ppcre:regex-replace-all "[^\\s\\w]" (string-downcase guess) " ")
      " ")
     ""))))

(defun correct-answer-p (question guess)
  (let ((normal-guess (normalize-guess guess)))
    (member normal-guess (mapcar 'normalize-guess (answers-of question)) :test #'string=)))

;;; Trivia scores
(defun load-trivia-scores (module)
  (with-open-file (inf (data-path "trivia-scores.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (setf (scores-of module) (if inf
                                 (read inf nil)
                                 nil))))

(defun save-trivia-scores (module)
  (with-open-file (ouf (data-path "trivia-scores.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write (scores-of module) :stream ouf)
    (terpri ouf)))

(defun user-trivia-score (module user)
  (length (rest (assoc (normalize-nick user) (scores-of module)
                       :test #'string-equal))))

(defun already-answered-p (module user question)
  (member (id-of question)
          (rest (assoc (normalize-nick user)
                       (scores-of module)
                       :test #'string-equal))))

(defun add-correct-answer (module user question)
  (let ((user-score (assoc (normalize-nick user) (scores-of module)
                           :test #'string-equal)))
    (if user-score
        (setf (cdr user-score) (cons (id-of question) (cdr user-score)))
        (push (list (normalize-nick user) (id-of question))
              (scores-of module)))
    (save-trivia-scores module)))

;;; Channel questions (questions active on a channel)
(defclass channel-question ()
  ((channel :accessor channel-of :initarg :channel)
   (time :accessor time-of :initarg :time)
   (question :accessor question-of :initarg :question)))

(defun channel-trivia-question (module channel)
  (second (assoc channel (channel-questions-of module) :test #'string-equal)))

(defun channel-question-expired (module channel)
  (let ((current-q (channel-trivia-question module channel)))
    (or (null current-q)
        (> (- (get-universal-time) (time-of current-q)) 60))))

(defun new-channel-question (module channel)
  (deactivate-channel-question module channel)

  (let* ((new-q (pop-trivia-question module))
         (channel-q (make-instance 'channel-question
                                   :channel channel
                                   :time (get-universal-time)
                                   :question new-q)))
    (push (list channel channel-q) (channel-questions-of module))
    new-q))

(defun deactivate-channel-question (module channel)
  (setf (channel-questions-of module)
        (delete channel (channel-questions-of module)
                :key #'first
                :test #'string=)))

;;; Trivia Queue
(defun populate-trivia-queue (module)
  (setf (queue-of module)
        (mapcar (lambda (idx)
                  (elt (questions-of module) idx))
                (let ((count (length (questions-of module))))
                  (make-random-list count count)))))

(defun pop-trivia-question (module)
  (unless (queue-of module)
    (populate-trivia-queue module))
  (pop (queue-of module)))

;;; Trivia game
(defun request-new-question (module channel output)
  (let ((current-q (channel-trivia-question module channel)))
    (cond
      ((trivia-questions-empty-p module)
       (format output "There are no trivia questions.~%"))
      ((channel-question-expired module channel)
       (when current-q
         (format output "The answer was: ~a~%"
                 (first (answers-of (question-of current-q)))))
       (let ((new-q (new-channel-question module channel)))
         (format output "~a. ~a" (id-of new-q) (text-of new-q))))
      (t
       (format output "~a. ~a"
               (id-of (question-of current-q))
               (text-of (question-of current-q)))))))

(defun guess-answer (module channel user guess output)
  (let* ((channel-q (channel-trivia-question module channel)))
    (when (and channel-q
               (correct-answer-p (question-of channel-q) guess))
      (cond
        ((already-answered-p module user (question-of channel-q))
         ;; Correct, but they've already answered the question
         (format output "~a answered correctly (~a point~:p)"
                 user
                 (user-trivia-score module user)))
        (t
         ;; New correct answer - give them a point
         (add-correct-answer module user (question-of channel-q))
         (format output "Point goes to ~a (~a point~:p)"
                   user
                   (user-trivia-score module user))))

      (deactivate-channel-question module channel))))

(defun display-trivia-score (module nick output)
  (let ((user (normalize-nick nick)))
    (format output "~a has answered ~a trivia question~:p correctly."
            user (user-trivia-score module user))))

(defun top-trivia-scores (module output)
  (let ((scores (sort (loop for tuple in (scores-of module)
                         as nick = (first tuple)
                         as score = (length (rest tuple))
                         collect (list nick score))
                      #'> :key #'second)))
    (loop
       for tuple in scores
       for place from 1 upto 5
       do (format output "~a. ~10a (~a)  "
                  place
                  (first tuple)
                  (second tuple)))))

;;; IRC interface to trivia
(defmethod initialize-module ((module trivia-module) config)
  (load-trivia-questions module)
  (load-trivia-scores module)
  (setf (queue-of module) nil))

(defmethod handle-message ((module trivia-module)
                           (message irc:irc-privmsg-message))
  (when (message-target-is-channel-p message)
    (let ((response (with-output-to-string (str)
                      (guess-answer module
                                    (first (arguments message))
                                    (source message)
                                    (second (arguments message))
                                    str))))
      (when (string/= response "")
        (reply-to message "~a" response))))

  ;; Answering a question does not consume the message
  nil)

(defmethod handle-command ((module trivia-module)
                           (cmd (eql 'trivia))
                           message args)
  "trivia - ask a new trivia question"
  (cond
    ((null args)
     (reply-to message "~a"
               (with-output-to-string (str)
                 (request-new-question module
                                       (first (arguments message))
                                       str))))
    ((string-equal "--score" (first args))
     (reply-to message "~a"
               (with-output-to-string (str)
                 (display-trivia-score module
                                       (or (second args)
                                           (source message))
                                       str))))
    ((string-equal "--top" (first args))
     (reply-to message "~a"
               (with-output-to-string (str)
                 (top-trivia-scores module str))))
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
     (let ((id (add-trivia-question module (join-to-string #\space args))))
       (if id
           (reply-to message "Question #~a created." id)
           (reply-to message "Bad question format."))))))

(defmethod handle-command ((module trivia-module)
                           (cmd (eql 'edittrivia))
                           message args)
  "edittrivia <question #> <question>[.?] <answer>. [<answer>.  ...] - replace the trivia question and answers"
  (if (null args)
      (reply-to message "Usage: ~~edittrivia <question #> <question>? answer. [<answer>. ...]")
      (let ((q-num (parse-integer (first args) :junk-allowed t)))
        (if (or (null q-num)
                (null (retrieve-trivia-question module q-num)))
            (reply-to message "That's not a valid question number.")
            (progn
              (edit-trivia-question module q-num (join-to-string #\space (rest args)))
              (reply-to message "Question #~a edited." q-num))))))

(defmethod handle-command ((module trivia-module)
                           (cmd (eql 'deltrivia))
                           message args)
  "deltrivia <question #> - delete a trivia question from the database"
  (if (null args)
      (reply-to message "Usage: ~~deltrivia <question #>")
      (let ((q-num (parse-integer (first args) :junk-allowed t)))
        (if (or (null q-num)
                (null (retrieve-trivia-question module q-num)))
            (reply-to message "That's not a valid question number.")
            (progn
              (delete-trivia-question module q-num)
              (reply-to message "Question #~a deleted." q-num))))))
