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

(defmodule typist typist-module ("typist")
  (texts :accessor texts-of
         :documentation "All the texts, adjustable vector of typist-texts")
  (history :accessor history-of :initform nil
          :documentation "Alist of correct answers of all the users who have played, (NICK TYPIST-TRIAL...)")
  (trials :accessor trials-of :initform nil
          :documentation "Alist of active trials by user (USER TYPIST-TRIAL)"))

(defclass typist-text ()
  ((id :accessor id-of :initarg :id
       :documentation "Unique identifier for the text")
   (body :accessor body-of :initarg :body
         :documentation "The body of the text")
   (source :accessor source-of :initarg :source
           :documentation "The source of the text")))

(defclass typist-trial ()
  ((timestamp :accessor timestamp-of :initarg :timestamp)
   (text-id :accessor text-id-of :initarg :text-id)
   (length :accessor length-of :initarg :length)
   (errors :accessor errors-of :initarg :errors)
   (duration :accessor duration-of :initarg :duration)))

(defparameter +characters-per-word+ 5)

(defmethod print-object ((object typist-text)
                         stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~d. ~s" (id-of object)

 (string-limit (body-of object) 20))))

(defun deserialize-typist-text (form)
  (make-instance 'typist-text
                 :id (first form)
                 :body (string-limit (second form) 255)
                 :source (third form)))

(defun serialize-typist-text (text)
  `(,(id-of text)
     ,(body-of text)
     ,@(source-of text)))

(defun load-typist-texts (module)
  (with-open-file (inf (data-path "typist-texts.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (cond
      (inf
       (let ((texts (read inf nil)))
         (setf (texts-of module)
               (make-array (length texts)
                           :adjustable t
                           :fill-pointer t))
         (map-into (texts-of module) 'deserialize-typist-text texts)
         (loop
            for text across (texts-of module)
            as idx from 1
            when (null (id-of text))
            do (setf (id-of text) idx))))
      (t
       (setf (texts-of module)
             (make-array 0 :adjustable t :fill-pointer t))))))

(defun save-typist-texts (module)
  (write-to-file (data-path "typist-texts.lisp")
                 (map 'list 'serialize-typist-text (texts-of module))))

(defun typist-texts-empty-p (module)
  (zerop (length (texts-of module))))

(defun edit-typist-text (module q-num body)
  (let* ((text (aref (texts-of module) q-num))
         (split (ppcre:split "\\s*/\\s*" body :limit 2)))
    (setf (body-of text) (first split))
    (setf (source-of text) (second split))
    (save-typist-texts module)
    nil))

(defun add-typist-text (module body)
  (let* ((new-id (1+ (reduce #'max (map 'vector 'id-of (texts-of module))))))
    ;; insert at end of database
    (vector-push-extend (make-instance 'typist-text :id new-id) (texts-of module))
    (edit-typist-text module new-id body)
    new-id))

(defun delete-typist-text (module q-num)
  (let* ((idx (1- q-num))
         (doomed-q (aref (texts-of module) idx)))
    (setf (texts-of module)
          (delete doomed-q (texts-of module)))
    (save-typist-texts module)
    doomed-q))

(defun levenshtein-distance (src dst)
  (let* ((m (length src))
         (n (length dst))
         (d (make-array (list (1+ m) (1+ n)))))
    (loop for i from 0 upto m do
         (setf (aref d i 0) i))
    (loop for j from 0 upto n do
         (setf (aref d 0 j) j))
    (loop for j from 1 upto n do
         (loop for i from 1 upto m do
              (setf (aref d i j) (if (char= (char src (1- i))
                                            (char dst (1- j)))
                                     (aref d (1- i) (1- j))
                                     (min (1+ (aref d (1- i) j))
                                          (1+ (aref d i (1- j)))
                                          (1+ (aref d (1- i) (1- j))))))))
    (aref d m n)))

(defun gross-wpm (length time)
  (/ (/ length +characters-per-word+)
     (/ time +seconds-per-minute+)))

(defun net-wpm (length time errors)
  (/ (- (/ length +characters-per-word+) errors)
     (/ time +seconds-per-minute+)))

;;; Typist history
(defun deserialize-user-history (form)
  (list* (first form)
         (mapcar (lambda (f)
                   (make-instance 'typist-trial
                                  :timestamp (first f)
                                  :text-id (second f)
                                  :length (third f)
                                  :errors (fourth f)
                                  :duration (fifth f)))
                 (rest form))))

(defun serialize-user-history (history)
  `(,(first history)
     ,@(mapcar (lambda (trial)
                 (list (timestamp-of trial)
                       (text-id-of trial)
                       (length-of trial)
                       (errors-of trial)
                       (duration-of trial)))
               (rest history))))

(defun load-typist-history (module)
  (with-open-file (inf (data-path "typist-history.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (when inf
      (setf (history-of module)
            (loop
               for user-data = (read inf nil)
               while user-data
               collect (deserialize-user-history user-data))))))

(defun save-typist-history (module)
  (write-to-file (data-path "typist-history.lisp")
                 (map 'list #'serialize-user-history (history-of module))))

(defun add-typist-trial (module nick timestamp text-id length errors duration)
  (let ((user-data (assoc (normalize-nick nick) (history-of module)
                          :test #'string-equal))
        (new-trial (make-instance 'typist-trial
                                  :timestamp timestamp
                                  :text-id text-id
                                  :length length
                                  :errors errors
                                  :duration duration)))

    (if user-data
        (push new-trial (rest user-data))
        (push (list (normalize-nick nick) new-trial)
              (history-of module)))
    (save-typist-history module)))

;;; Typist game
(defun request-new-typist-trial (module user output)
  (let ((text (second (assoc (normalize-nick user) (trials-of module)
                             :test #'string-equal))))
    
    (cond
      ((typist-texts-empty-p module)
       (format output "There are no texts to test with.~%"))
      (text
       (format output "(text #~d) ~a" (id-of text) (body-of text)))
      (t
       (let ((new-text (random-elt (texts-of module))))
         (push (list user new-text (get-universal-time)) (trials-of module))
         (format output "(text #~d) ~a" (id-of new-text) (body-of new-text)))))))

(defun process-typist-response (module user sample output)
  (let* ((trial (assoc user (trials-of module) :test #'string-equal))
         (text (second trial))
         (started (third trial)))
    (when text
      (let* ((errors (levenshtein-distance sample (body-of text)))
             (now (get-universal-time))
             (duration (- now started)))
        (add-typist-trial module user now (id-of text)
                          (length sample)
                          errors
                          duration)
        (setf (trials-of module)
              (delete user (trials-of module)
                      :key #'first
                      :test #'string=))
        (format output "You typed that in ~,1f second~:p with ~,1d error~:p, for a net speed of ~,1f WPM"
                duration
                errors
                (net-wpm (length sample) duration errors))))))

(defun display-typist-score (module nick output)
  (let* ((user (normalize-nick nick))
         (history (rest (assoc user (history-of module) :test #'string-equal)))
         (last-trial (first history)))
    (if history
        (let ((trial-count 0)
              (last-10-length 0)
              (last-10-time 0)
              (last-10-errors 0)
              (total-length 0)
              (total-time 0)
              (total-errors 0)
              (last-wpm (net-wpm (length-of last-trial)
                                 (duration-of last-trial)
                                 (errors-of last-trial)))
              (best-wpm 0))
          (loop
             for trial in history
             as num from 0
             as wpm = (net-wpm (length-of trial)
                               (duration-of trial)
                               (errors-of trial))
             do
               (incf trial-count)
               (when (< num 10)
                 (incf last-10-length (length-of trial))
                 (incf last-10-time (duration-of trial))
                 (incf last-10-errors (errors-of trial)))
               (incf total-length (length-of trial))
               (incf total-time (duration-of trial))
               (incf total-errors (errors-of trial))
               (when (> wpm best-wpm)
                 (setf best-wpm wpm)))
          (format output "~a :: Trials: ~d, Last 10: ~,1f wpm, All Time: ~,1f wpm, Last: ~,1f wpm, Best: ~,1f wpm"
                  nick
                  trial-count
                  (net-wpm last-10-length last-10-time last-10-errors)
                  (net-wpm total-length total-time total-errors)
                  last-wpm
                  best-wpm))
        (format output "~a has not done any typing trials." nick))))

;;; IRC interface to typist
(defmethod initialize-module ((module typist-module) config)
  (load-typist-texts module)
  (load-typist-history module)
  (setf (trials-of module) nil))

(defmethod handle-message ((module typist-module)
                           (message irc:irc-privmsg-message))
  (when (not (message-target-is-channel-p message))
    (let ((response (with-output-to-string (str)
                      (process-typist-response module (source message)
                                               (second (arguments message))
                                               str))))
      (when (string/= response "")
        (reply-to message "~a" response))))

  ;; Answering a text does not consume the message
  nil)

(defmethod handle-command ((module typist-module)
                           (cmd (eql 'typist))
                           message args)
  "typist - ask a new typist text"
  (cond
    ((string-equal "--score" (first args))
     (reply-to message "~a"
               (with-output-to-string (str)
                 (display-typist-score module
                                       (or (second args)
                                           (source message))
                                       str))))
    ((not (null args))
     (reply-to message "typist                  - request a new typist trial")
     (reply-to message "typist --score [<nick>] - get score of user"))
    ((message-target-is-channel-p message)
     (reply-to message "You can only request a typist trial in a private message"))
    (t
     (irc:notice (connection message)
                 (source message)
                 (with-output-to-string (str)
                   (request-new-typist-trial module (source message) str))))))

(defun raid-fortune-files (start-idx dir-path output-path)
  (let ((result nil))
    (dolist (path (directory dir-path))
      (with-open-file (inf path :direction :input)
        (loop
           with accumulator = nil
           for line = (read-line inf nil)
           while line do
             (if (and (string/= line "")
                      (char= (char line 0) #\%))
                 (let ((text (string-trim '(#\space)
                                          (ppcre:regex-replace-all (ppcre:create-scanner "\\s+" :multi-line-mode t)
                                                                   (join-to-string #\space (nreverse accumulator))
                                                                   " "))))
                   (setf accumulator nil)
                   (when (and (< 150 (length text) 475)
                              (every #'graphic-char-p text))
                     (push (list start-idx text) result)
                     (incf start-idx)))
                 (push line accumulator)))))
    (with-open-file (ouf output-path :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (write (nreverse result) :stream ouf)))
  (values))