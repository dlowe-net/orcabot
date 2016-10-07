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

(defun action-to (message fmt &rest args)
  (let* ((raw-response (format nil "~?" fmt args))
         (raw-response-lines (ppcre:split "\\n" raw-response))
         (responses (mapcan (lambda (line)
                              (wrap-string line 405))
                            raw-response-lines)))
    (cond
      ((message-target-is-channel-p message)
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
      ((message-target-is-channel-p message)
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

(defun valid-nick-p (nick)
  (and (ppcre:scan "^[A-Za-z_\\{}[\\]^`|][A-Za-z_\\{}[\\]^`|0-9-]\*$" nick)
       t))

(defun irc-downcase (s)
  "Downcase the string using special IRC rules, as specified in section 2.2 of RFC 2812."
  (map 'string
       (lambda (c)
         (case c
           (#\[ #\{)
           (#\] #\})
           (#\\ #\|)
           (#\~ #\^)
           (t
            (char-downcase c))))
       s))

(defun irc-string-equal (a b)
  "Compare nicks for equality using specially downcased versions of the nicks."
  (string= (irc-downcase a) (irc-downcase b)))

(defun normalize-nick (nick)
  "Remove trailing numbers and everything after an underscore or dash, then
downcase what is left. Used for comparing nicks for equality."
  (irc-downcase (ppcre:regex-replace "(?:[-_].*|\\d+$)" nick "")))

(defun message-target-is-channel-p (message)
  (find (char (first (arguments message)) 0) "#+"))

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
