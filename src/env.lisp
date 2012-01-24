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

(in-package #:orca)

(defmodule env env-module ("env" "take" "share" "steal" "release")
  (environments :accessor environments-of :initform nil)
  (leases :accessor leases-of :initform nil)
  (statuses :accessor statuses-of :initform nil))

(defmethod initialize-module ((module env-module) config)
  (load-env-data module))

(defun save-env-data (module)
  (with-open-file (ouf (orca-path "data/pss-envs.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-line ";; Environments" ouf)
    (write (environments-of module) :stream ouf)
    (terpri ouf)
    (write-line ";; Leases" ouf)
    (write (leases-of module) :stream ouf)
    (terpri ouf)
    (write-line ";; Statuses" ouf)
    (write (statuses-of module) :stream ouf)
    (terpri ouf)))

(defun load-env-data (module)
  (with-open-file (inf (orca-path "data/pss-envs.lisp") :direction :input)
    (setf (environments-of module) (read inf nil nil))
    (setf (leases-of module) (read inf nil nil))
    (setf (statuses-of module) (read inf nil nil))))

(defun expire-env-leases (module)
  (let ((now (get-universal-time)))
    (setf (leases-of module) (delete-if (lambda (lease)
                                    (<= (fourth lease) now))
                                  (leases-of module)))))

(defun describe-time-left (span)
  (cond
    ((> span 86400)
     (format nil "~ad" (round span 86400)))
    ((> span 3600)
     (format nil "~ah" (round span 3600)))
    ((> span 60)
     (format nil "~am" (round span 60)))
    (t
     (format nil "~as" span))))

(defun parse-expire-time (str)
  (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings "^(\\d+)([dhms]?)$" str)
    (when match
      (let ((num (parse-integer (aref regs 0))))
        (+ (get-universal-time)
           (if (zerop (length (aref regs 1)))
               (* num 60)
               (ecase (char (aref regs 1) 0)
                 (#\d (* num 86400))
                 (#\h (* num 3600))
                 (#\m (* num 60))
                 (#\s num))))))))

(defun query-leases (module env-name leases)
  (let ((matching (remove env-name leases :test-not #'string-equal :key #'cadr))
        (status (cdr (assoc env-name (statuses-of module) :test #'string-equal))))
    (if matching
        (with-output-to-string (str)
          (format str "~a~@[(~a)~] used by: " env-name status)
          (loop
             with end = (last matching)
             for match-el on matching
             for match = (car match-el)
             do
             (format str "~a (~@[~a/~]~a)" (first match)
                     (third match)
                     (describe-time-left (- (fourth match) (get-universal-time))))
             (unless (eq match-el end)
               (format str ", "))))
        (format nil "~a~@[(~a)~] is currently free" env-name status))))

(defun remove-all-env-leases (module env-name)
  (setf (leases-of module) (remove env-name (leases-of module) :test #'string-equal :key #'second)))

(defun remove-env-lease (module env-name nick)
  (setf (leases-of module) (remove-if (lambda (lease)
                                  (and (string-equal (second lease) env-name)
                                       (string-equal (first lease) nick)))
                                (leases-of module))))

(defun create-lease (module env-name nick time-str activity)
  (let ((expire-time (parse-expire-time time-str)))
    (cond
      ((not (member env-name (environments-of module) :test #'string-equal))
       (format nil "Can't find environment ~a" env-name))
      ((null expire-time)
       (format nil "Sorry, didn't understand ~a as an amount of time" time-str))
      (t
       (remove-env-lease module env-name nick)
       (push (list nick env-name activity expire-time) (leases-of module))
       (save-env-data module)
       (format nil "~a taken by ~a for ~a"
               env-name
               nick
               (describe-time-left (- expire-time (get-universal-time))))))))

(defun env-has-lease-p (module env-name)
  (find env-name (leases-of module)
        :key #'second
        :test #'string-equal))

(defun release-lease (module env-name nick)
  (cond
    ((not (member env-name (environments-of module) :test #'string-equal))
     (format nil "Can't find environment ~a" env-name))
    (t
     (remove-env-lease module env-name nick)
     (save-env-data module)
     (format nil "~a released by ~a" env-name nick))))

(defun set-env-status (module env-name status)
  (cond
    ((not (member env-name (environments-of module) :test #'string-equal))
     (format nil "Can't find environment ~a" env-name))
    ((zerop (length status))
     (setf (statuses-of module) (remove (assoc env-name (statuses-of module) :test #'string-equal)
                                      (statuses-of module)))
     (save-env-data module)
     (format nil "~a status removed" env-name))
    (t
     (let ((tuple (assoc env-name (statuses-of module) :test #'string-equal)))
       (if tuple
           (setf (cdr tuple) status)
           (setf (statuses-of module) (acons env-name status (statuses-of module)))))
     (save-env-data module)
     (format nil "~a status is now: ~a" env-name status))))

(defun send-env-usage (message)
  (flet ((send (text)
           (irc:notice (connection message) (source message) text)))
    (send "~env                                 - list known environments")
    (send "~env <envname>                       - display environment info")
    (send "~take <envname> <time> [<activity>]  - lease the environment")
    (send "~share <envname> <time> [<activity>] - lease the environment with someone else")
    (send "~steal <envname> <time> [<activity>] - take the environment from someone else")
    (send "~release <envname>                   - release your lease on environment")
    (send "~update <envname> [<status>]         - set the environment status")
    (send "~env add <envname>                   - add a new environment")
    (send "~env remove <envname>                - remove an environment")
    (send "~env help                            - display this help")))


(defmethod handle-command ((module env-module) (cmd (eql 'env))
                           message args)
  "env - manage environment leases and statuses"
  (expire-env-leases module)
  (let ((env-name (first args))
        (subcmd (second args)))
    (cond
      ((null args)
       (reply-to message "~{~a~^, ~}" (environments-of module)))
      ((string-equal env-name "help")
       (send-env-usage message))
      ((null (cdr args))
       ;; query leases
       (if (member env-name (environments-of module) :test #'string-equal)
           (reply-to message (query-leases module (first args) (leases-of module)))
           (reply-to message "Can't find environment ~a" env-name)))
      ((string-equal subcmd "take")
       (let ((lease (env-has-lease-p module env-name)))
         (if lease
           (reply-to message "~a is being leased by ~a for ~a.  You may share or steal the environment.")
           (reply-to message (create-lease module env-name (source message)
                                           (third args)
                                           (join-string #\space (cdddr args)))))))
      ((string-equal subcmd "share")
       (reply-to message (create-lease module env-name (source message)
                                       (third args)
                                       (join-string #\space (cdddr args)))))
      ((string-equal subcmd "steal")
       (remove-all-env-leases module env-name)
       (reply-to message (create-lease module env-name (source message)
                                       (third args)
                                       (join-string #\space (cdddr args)))))

      ((string-equal subcmd "release")
       (reply-to message (release-lease module env-name (source message))))
      ((string-equal subcmd "status")
       (reply-to message (set-env-status module env-name (join-string #\space (cddr args)))))
      ((string-equal subcmd "add")
       (cond
         ((member env-name (environments-of module) :test #'string-equal)
          (reply-to message "That environment is already known."))
         (t
          (push env-name (environments-of module))
          (setf (environments-of module) (sort (environments-of module) #'string<))
          (save-env-data module)
          (reply-to message "Added ~a." env-name))))
      ((string-equal subcmd "remove")
       (cond
         ((not (member env-name (environments-of module) :test #'string-equal))
          (reply-to message "Can't find environment ~a" env-name))
         (t
          (setf (environments-of module) (remove env-name (environments-of module) :test #'string-equal))
          (save-env-data module)
          (reply-to message "Removed ~a from environments." env-name))))
      (t
       (send-env-usage message)))))

(defmethod handle-command ((module env-module) (cmd (eql 'take)) message args)
  "take <envname> <time> [<activity>] - Acquire a lease on an environment"
  (expire-env-leases module)
  (let ((lease (env-has-lease-p module (first args))))
    (if lease
        (reply-to message "~a is being leased by ~a for ~a.  You may share or steal the environment.")
        (reply-to message (create-lease module (first args) (source message)
                                        (second args)
                                        (join-string #\space (cddr args)))))))

(defmethod handle-command ((module env-module) (cmd (eql 'share)) message args)
  "share <envname> <time> [<activity>] - Share an environment with someone else"
  (expire-env-leases module)
  (reply-to message (create-lease module (first args) (source message)
                                  (second args)
                                  (join-string #\space (cddr args)))))

(defmethod handle-command ((module env-module) (cmd (eql 'steal)) message args)
  "steal <envname> <time> [<activity>] - Take an environment from someone else"
  (expire-env-leases module)
  (remove-all-env-leases module (first args))
  (reply-to message (create-lease module (first args) (source message)
                                  (second args)
                                  (join-string #\space (cddr args)))))

(defmethod handle-command ((module env-module) (cmd (eql 'release)) message args)
  "release <envname> - Release your lease on an environment"
  (expire-env-leases module)
  (reply-to message (release-lease module (first args) (source message))))

(defmethod handle-command ((module env-module) (cmd (eql 'update)) message args)
  "update <envname> [<status>]- Update an environment's status"
  (reply-to message (set-env-status module (first args) (join-string #\space (cdr args)))))
