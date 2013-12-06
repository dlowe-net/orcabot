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

(defmodule parrot parrot-module ("parrot")
  (parrots :accessor parrots-of :initform (make-hash-table :test 'equalp))
  (save-counter :accessor save-counter-of :initform 0))

(defmethod initialize-module ((module parrot-module) config)
  (setf (save-counter-of module)
        (or (second (assoc 'parrot-save-lines config)) 100))
  (load-parrots module))

(defun save-parrots (module)
  (with-open-file (ouf (orcabot-path "data/parrots.lisp")
                       :direction :output
                       :if-exists :rename-and-delete
                       :if-does-not-exist :create)
    (dolist (parrot-nick (sort (hash-table-keys (parrots-of module)) #'string<))
      (let ((parrot (gethash parrot-nick (parrots-of module))))
        (format ouf "(parrot ~s ~s)~%"
                parrot-nick
                (loop
                   for key in (hash-table-keys parrot)
                   collect (list key (gethash key parrot))))))))

(defun load-parrots (module)
  (clrhash (parrots-of module))
  (let ((*package* (find-package "ORCABOT")))
    (with-open-file (inf (orcabot-path "data/parrots.lisp"))
      (loop
         for parrot-spec = (read inf nil)
         while parrot-spec
         when (eql (first parrot-spec) 'parrot)
         do
           (let ((parrot (make-hash-table :test 'equal)))
             (setf (gethash (second parrot-spec) (parrots-of module)) parrot)
             (dolist (tuple (third parrot-spec))
               (setf (gethash (first tuple) parrot) (second tuple))))))))

(defmethod handle-message ((module parrot-module)
                           (message irc:irc-privmsg-message))

  (parrot-learn module (source message) (second (arguments message)))
  (when (>= (save-counter-of module) 100)
    (save-parrots module)
    (setf (save-counter-of module) 0)))

(defmethod handle-command ((module parrot-module)
                           (cmd (eql 'parrot))
                           message args)
  (cond
    ((null args)
     (reply-to message "Usage: parrot <nick>"))
    ((null (gethash (normalize-nick (first args)) (parrots-of module)))
     (reply-to message "Never heard of ~a." (first args)))
    (t
     (reply-to message "~a" (parrot-speak module (normalize-nick (first args)))))))

(defun markov-learn (corpus text)
  (let ((words (cl-ppcre:split "\\s+" text)))
    (loop for (first second) on words by #'cdr
       while (and first second)
       do (push second (gethash (list first) corpus)))))

(defun markov-generate (corpus count)
  (let ((result (random-elt (hash-table-keys corpus))))
    (loop
       repeat count
       for branches = (gethash (list (first result)) corpus)
       while branches
       do (push (random-elt branches) result))
    (nreverse result)))

(defun parrot-learn (module nick text)
  (let ((parrot (gethash nick (parrots-of module))))
    (unless parrot
      (setf parrot (make-hash-table :test 'equal))
      (setf (gethash nick (parrots-of module)) parrot))
    (markov-learn parrot text)))

(defun parrot-speak (module nick)
  (let ((parrot (gethash nick (parrots-of module))))
    (if parrot
        (format nil "<~a> ~a" nick (join-to-string " " (markov-generate parrot 100)))
        (format nil "Never heard of ~a" nick))))

;;; The code below this line is for convenience
(defun parrots-learn-from-line (module line)
  (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings
       "<span class=\"irc-black\">&lt;([^[&_-]+)[^&]*&gt; (.*)</span><br />"
       line :sharedp t)
    (when match
      (parrot-learn module (aref regs 0) (aref regs 1)))))

(defun parrots-learn-from-file (module path)
  (with-open-file (inf path :direction :input)
    (loop
       for line = (read-line inf nil)
       while line
       do (parrots-learn-from-line module line))))

(defun parrots-learn-from-dir (module dir-path)
  (dolist (file-path (directory dir-path))
    (format t "Learning from ~a~%" file-path)
    (parrots-learn-from-file module file-path)))

(defun textify-irc-logs (dir-path)
  (dolist (inf-path (directory dir-path))
    (format t "Textifying ~a~%" inf-path)
    (with-open-file (inf inf-path)
      (with-open-file (ouf (make-pathname
                            :type "txt"
                            :defaults inf-path)
                           :direction :output
                           :if-exists :rename-and-delete
                           :if-does-not-exist :create)
        (loop
           for line = (read-line inf nil)
           while line
           do
             (cl-ppcre:register-groups-bind (hour minute nick message)
                 ("<span class=\"irc-date\">\\[(\\d+):(\\d+)\\]</span> <span class=\"irc-black\">&lt;([^[&_-]+)[^&]*&gt; (.*)</span><br />" line :sharedp t)
               (when (and hour minute)
                 (format ouf "[~2d:~2d] <~a> ~a~%"
                         (parse-integer hour)
                         (parse-integer minute)
                         nick
                         message)))
             (cl-ppcre:register-groups-bind (hour minute message)
                 ("<span class=\"irc-date\">\\[(\\d+):(\\d+)\\]</span> <span class=\"irc-brick\">(\\* .*)</span><br />" line :sharedp t)
               (when (and hour minute)
                 (format ouf "[~2d:~2d] ~a~%"
                         (parse-integer hour)
                         (parse-integer minute)
                         message))))))))
