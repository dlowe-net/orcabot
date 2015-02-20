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

(defvar *orcabot-data-root-pathname* nil)
(defparameter *orcabot-static-root-pathname*
  (merge-pathnames #p"data/"
                   (asdf:component-pathname (asdf:find-system "orcabot"))))

(define-condition keepalive-failed () ())
(define-condition orcabot-exiting () ())
(define-condition no-such-module (error) ())

(defvar *command-funcs* (make-hash-table :test 'equalp))

(defun static-path (filename)
  "Returns a pathname referring to the static data directory in the
project root."
  (merge-pathnames filename *orcabot-static-root-pathname*))

(defun data-path (filename)
  "Returns a pathname within the data directory specified at startup."
  (merge-pathnames filename *orcabot-data-root-pathname*))

(defun write-to-file (path object)
  (with-open-file (ouf (data-path path)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write object :stream ouf)
    (terpri ouf)))

(defun http-to-file (url path)
  (let ((inf (drakma:http-request url :want-stream t)))
    (when inf
      (let ((byte-stream (flexi-streams:flexi-stream-stream inf)))
        (with-open-file (ouf path
                             :element-type '(unsigned-byte 8)
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (loop
             with buf = (make-array #x2000 :element-type '(unsigned-byte 8))
             for read-bytes = (read-sequence buf byte-stream)
             while (= read-bytes #x2000)
             do (write-sequence buf ouf)
             finally (write-sequence buf ouf :end read-bytes))))
      t)))

(defun join-to-string (delimiter seq)
  "Returns a string with the printed elements of SEQ seperated by the
printed elements of DELIMITER."
  (with-output-to-string (result)
    (cond
      ((consp seq)
        (loop for el on seq do
             (write (car el) :stream result :escape nil)
             (when (cdr el)
               (write delimiter :stream result :escape nil))))
      ((plusp (length seq))
        (loop 
           for idx from 0 upto (- (length seq) 2)
           do
             (write (elt seq idx) :stream result :escape nil)
             (write delimiter :stream result :escape nil)
           finally (write (elt seq (1- (length seq))) :stream result :escape nil))))))

(defun string-limit (str max-len)
  (string-trim '(#\space)
               (if (< (length str) max-len)
                   str
                   (let ((pivot (position-if-not #'alphanumericp
                                                 str
                                                 :from-end t
                                                 :end max-len)))
                     (concatenate 'string
                                  (subseq str 0 (or pivot max-len))
                                  "...")))))

(defun wrap-string (string width)
  "Given an initial string, returns the string broken up into lines breaking on word boundaries."
  (loop
     for raw-wrap-pos = width then (+ wrap-pos width)
     until (> raw-wrap-pos (length string))
     for wrap-pos = (or (position-if-not #'alpha-char-p string :end raw-wrap-pos :from-end t)
                        raw-wrap-pos)
     for start-pos = 0 then (1+ wrap-pos)
     collect (string-trim " " (subseq string start-pos wrap-pos))
     into result
     finally (return (nconc result
                            (list (string-trim " "
                                               (subseq string (or wrap-pos 0))))))))


(defun action-to (message fmt &rest args)
  (let* ((raw-response (format nil "~?" fmt args))
         (raw-response-lines (ppcre:split "\\n" raw-response))
         (responses (mapcan (lambda (line)
                              (wrap-string line 405))
                            raw-response-lines)))
    (cond
      ((char= #\# (char (first (arguments message)) 0))
       (dolist (line responses)
         (when (string/= line "")
           (irc::action (connection message) (first (arguments message)) line))))
      (t
       (dolist (line responses)
         (when (string/= line "")
           (irc::action (connection message) (source message) line)))))))

(defun reply-to (message fmt &rest args)
  (let* ((raw-response (format nil "~?" fmt args))
         (raw-response-lines (ppcre:split "\\n" raw-response))
         (responses (mapcan (lambda (line)
                              (wrap-string line 405))
                            raw-response-lines)))
    (cond
      ((char= #\# (char (first (arguments message)) 0))
       (dolist (line responses)
         (when (string/= line "")
           (irc:privmsg (connection message) (first (arguments message)) line))))
      (t
       (dolist (line responses)
         (when (string/= line "")
           (irc:privmsg (connection message) (source message) line)))))))

(defun authentication-credentials (host)
  (flet ((read-word (stream)
           (when (peek-char t stream nil)
             (with-output-to-string (s)
               (loop
                  for c = (read-char stream nil)
                  while (and c
                             (char/= c #\newline)
                             (char/= c #\space)) do
                    (princ c s))))))
    (let ((found-machine nil)
          (result nil))
      (with-open-file (inf (merge-pathnames (user-homedir-pathname)
                                            ".netrc")
                           :direction :input
                           :if-does-not-exist nil)
        (when inf
          (loop
             for key = (read-word inf)
             as val = (read-word inf)
             while val do
             (cond
               ((string-equal key "machine")
                (setf found-machine (string-equal val host)))
               (found-machine
                (push val result)
                (push (intern (string-upcase key) :keyword) result)))))
        result))))

(defun shorten-nick (full-nick)
  (ppcre:scan-to-strings "[A-Za-z]+" full-nick))

(defun normalize-nick (nick)
  "Remove trailing numbers and everything after an underscore or dash.
Used for comparing nicks for equality."
  (string-downcase (ppcre:regex-replace "(?:[-_].*|\\d+$)" nick "")))

(defun message-target-is-channel-p (message)
  (find (char (first (arguments message)) 0) "#+"))

(defun all-matches-register (regex target-string register
                             &key (start 0)
                             (end (length target-string))
                             (sharedp nil))
  (let ((substr-fn (if sharedp #'ppcre::nsubseq #'subseq))
        (result-list nil))
      (ppcre:do-scans (start end reg-start reg-end
                             regex target-string
                             result-list
                             :start start :end end)
        (push (funcall substr-fn
                       target-string
                       (aref reg-start register)
                       (aref reg-end register))
              result-list))))

(defun switch-person (str)
  (cl-ppcre:regex-replace-all
   (cl-ppcre:create-scanner "\\b(mine|me|my|I am|I'm|I|you are|you're|yours|your|you)\\b" :case-insensitive-mode t)
   str
   (lambda (target start end match-start match-end reg-starts reg-ends)
     (declare (ignore start end reg-starts reg-ends))
     (let ((match (make-array (list (- match-end match-start)) :element-type 'character :displaced-to target :displaced-index-offset match-start)))
       (cond
         ((string-equal "I" match)
          "you")
         ((string-equal "me" match)
          "you")
         ((string-equal "my" match)
          "your")
         ((string-equal "I am" match)
          "you are")
         ((string-equal "I'm" match)
          "you're")
         ((string-equal "mine" match)
          "yours")
         ((string-equal "you" match)
          "I")
         ((string-equal "your" match)
          "my")
         ((string-equal "yours" match)
          "mine")
         ((string-equal "you're" match)
          "I'm")
         ((string-equal "you are" match)
          "I am"))))))

(defun make-random-list (length max)
  "Generate a non-repeating list of random numbers of length LENGTH.  The maxim\
um value of the random numbers is MAX - 1."
  (declare (fixnum length max))
  (unless (plusp length)
    (error "LENGTH may not be negative."))
  (unless (plusp max)
    (error "Can't generate negative numbers."))
  (unless (<= length max)
    (error "Can't generate a non-repeating list when LENGTH > MAX"))
  (let ((unused (make-array max :element-type 'fixnum)))
    ;; create an array with each element set to its index
    (dotimes (idx max)
      (setf (aref unused idx) idx))
    ;; select a random index to pull from the array, then set the number
    ;; at the index to the last selectable element in the array.  Continue
    ;; until we have the requisite list length
    (loop for result-size from 0 upto (1- length)
          as num = (random (- max result-size))
          collect (aref unused num)
          do (setf (aref unused num) (aref unused (- max result-size 1))))))

(defun describe-duration (span)
  (cond
    ((>= span 86400)
     (format nil "~ad" (round span 86400)))
    ((>= span 3600)
     (format nil "~ah" (round span 3600)))
    ((>= span 60)
     (format nil "~am" (round span 60)))
    (t
     (format nil "~as" span))))

(defun parse-expire-time (str)
  (or 
   (multiple-value-bind (match regs)
       (cl-ppcre:scan-to-strings "^(\\d+)([dhms]?)$" str)
     (when match
       (let ((num (parse-integer (aref regs 0))))
         (if (zerop (length (aref regs 1)))
             (* num 60)
             (ecase (char (aref regs 1) 0)
               (#\d (* num 86400))
               (#\h (* num 3600))
               (#\m (* num 60))
               (#\s num))))))
   (let* ((now (local-time:now))
          (parsed-ts (chronicity:parse str
                                       :context :future
                                       :now now
                                       :endian-preference :middle
                                       :guess :middle
                                       :ambiguous-time-range 8)))
     (when (and parsed-ts
                (timestamp< now parsed-ts))
       (local-time:timestamp-difference parsed-ts now)))))