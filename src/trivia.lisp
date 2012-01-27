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
  (queue :accessor queue-of :documentation "A queue of the questions to be asked")
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
  (with-open-file (inf (orcabot-path "data/trivia-questions.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (when inf
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
           do (setf (id-of question) idx))))))

(defun save-trivia-questions (module)
  (with-open-file (ouf (orcabot-path "data/trivia-questions.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)

    (write (map 'list 'serialize-trivia-question (questions-of module))
           :stream ouf)
    (terpri ouf)))

(defun add-trivia-question (module spec)
  (let* ((split-q (nth-value 1 (if (find #\? spec)
                                   (ppcre:scan-to-strings "\\s*(.*?\\?)\\s*(.+)" spec)
                                   (ppcre:scan-to-strings "\\s*(.*?\\.)\\s*(.+)" spec))))
         (answers (and split-q (ppcre:split "\\s*\\.\\s*" (elt split-q 1))))
         (new-id (1+ (reduce #'max (map 'vector 'id-of (questions-of module)))))
         (new-question (make-instance 'trivia-question
                                      :id new-id
                                      :text (first split-q)
                                      :answers answers)))
    ;; insert at end of database
    (vector-push-extend new-question (questions-of module))
    (save-trivia-questions module)
    new-id))

(defun edit-trivia-question (module q-num spec)
  (let* ((question (aref (questions-of module) q-num))
         (split-q (nth-value 1 (if (find #\? spec)
                                   (ppcre:scan-to-strings "\\s*(.*?\\?)\\s*(.+)" spec)
                                   (ppcre:scan-to-strings "\\s*(.*?\\.)\\s*(.+)" spec))))
         (answers (and split-q (ppcre:split "\\s*\\.\\s*" (elt split-q 1)))))
    (setf (text-of question) (elt split-q 0))
    (setf (answers-of question) answers)
    nil))

(defun delete-trivia-question (module q-num)
  (let* ((idx (1- q-num))
         (doomed-q (aref (questions-of module) idx)))
    (setf (questions-of module)
          (delete doomed-q (questions-of module)))
    (setf (queue-of module)
          (delete doomed-q (questions-of module)))
    (save-trivia-questions module)
    doomed-q))

(defun add-trivia-answer (module q-num answer)
  (let ((current-q (nth (1- q-num) (questions-of module))))
    (unless (member answer (answers-of current-q) :test #'string=)
      (setf (answers-of current-q) (append (answers-of current-q)
                                           (list answer)))
      (save-trivia-questions module))))

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

(defun correct-answer-p (question guess)
  (let ((normal-guess (normalize-guess guess)))
    (member normal-guess (mapcar 'normalize-guess (answers-of question)) :test #'string=)))

;;; Trivia scores
(defun load-trivia-scores (module)
  (with-open-file (inf (orcabot-path "data/trivia-scores.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (setf (scores-of module) (if inf
                                 (read inf nil)
                                 nil))))

(defun save-trivia-scores (module)
  (with-open-file (ouf (orcabot-path "data/trivia-scores.lisp")
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
      ((channel-question-expired module channel)
       (when current-q
         (format output "The answer was: ~a"
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
       do (format output "~a. ~10a (~a)"
                  place
                  (first tuple)
                  (second tuple)))))

;;; IRC interface to trivia
(defmethod initialize-module ((module trivia-module) config)
  (load-trivia-questions module)
  (load-trivia-scores module)
  (setf (queue-of module) nil))

(defmethod handle-message ((module trivia-module)
                           (type (eql 'irc:irc-privmsg-message))
                           message)
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
     (reply-to message "Question #~a created."
               (add-trivia-question module (join-string #\space args))))))

(defmethod handle-command ((module trivia-module)
                           (cmd (eql 'edittrivia))
                           message args)
  "edittrivia <question #> <question>[.?] <answer>. [<answer>.  ...] - replace the trivia question and answers"
  (if (null args)
      (reply-to message "Usage: ~~edittrivia <question #> <question>? answer. [<answer>. ...]")
      (let ((q-num (parse-integer (first args) :junk-allowed t)))
        (if (or (null q-num)
                (not (<= 1 q-num (length (questions-of module)))))
            (reply-to message "That's not a valid question number.")
            (progn
              (edit-trivia-question module q-num (join-string #\space (rest args)))
              (reply-to message "Question #~a edited." q-num))))))

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
              (delete-trivia-question module q-num)
              (reply-to message "Question #~a deleted." q-num))))))
