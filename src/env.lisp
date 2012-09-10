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

(defmodule env env-module ("env" "take" "share" "steal" "release" "update")
  (environments :accessor environments-of :initform nil)
  (leases :accessor leases-of :initform nil)
  (statuses :accessor statuses-of :initform nil))

(defclass lease ()
  ((nick :accessor nick-of :initarg :nick)
   (env :accessor env-of :initarg :env)
   (activity :accessor activity-of :initarg :activity)
   (expires :accessor expires-of :initarg :expires)
   (timer :accessor timer-of :initarg :timer :initform nil)))

(defmethod initialize-module ((module env-module) config)
  (load-env-data module))

(defmethod deinitialize-module ((module env-module))
  (dolist (lease (leases-of module))
    (when (timer-of lease)
      (iolib:remove-timer *event-base* (timer-of lease)))))

(defun save-env-data (module)
  (with-open-file (ouf (orcabot-path "data/pss-envs.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-line ";; Environments" ouf)
    (write (environments-of module) :stream ouf)
    (terpri ouf)
    (write-line ";; Leases" ouf)
    (write (mapcar
            (lambda (lease)
              (list (nick-of lease)
                    (env-of lease)
                    (activity-of lease)
                    (expires-of lease)))
            (leases-of module))
           :stream ouf)
    (write-line ";; Statuses" ouf)
    (write (statuses-of module) :stream ouf)
    (terpri ouf)))

(defun instantiate-lease-timer (module lease now)
  (when (and (expires-of lease)
             (> (expires-of lease) now))
    (setf (timer-of lease)
          (iolib:add-timer *event-base*
                           (lambda ()
                             (cl-irc:notice (conn-of module)
                                            (nick-of lease)
                                            (format nil "Your lease on ~a has expired."
                                                    (env-of lease))))
                           (- (expires-of lease) now)
                           :one-shot t))))

(defun load-env-data (module)
  (with-open-file (inf (orcabot-path "data/pss-envs.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (when inf
      (setf (environments-of module) (read inf nil nil))
      (setf (leases-of module)
            (loop for lease-data in (read inf nil)
               collect (make-instance 'lease
                                      :nick (first lease-data)
                                      :env (second lease-data)
                                      :activity (third lease-data)
                                      :expires (fourth lease-data))))
      (setf (statuses-of module) (read inf nil nil))
      ;; instantiate timers
      (let ((now (get-universal-time)))
        (dolist (lease (leases-of module))
          (instantiate-lease-timer module lease now))))))

(defun expire-env-leases (module)
  (let ((now (get-universal-time)))
    (setf (leases-of module) (delete-if (lambda (lease)
                                    (<= (expires-of lease) now))
                                  (leases-of module)))))

(defun parse-expire-time (str)
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
              (#\s num)))))))

(defun query-leases (module env-name leases)
  (let ((matching (remove env-name leases :test-not #'string-equal :key #'env-of))
        (status (cdr (assoc env-name (statuses-of module) :test #'string-equal))))
    (if matching
        (with-output-to-string (str)
          (format str "~a~@[(~a)~] used by: " env-name status)
          (loop
             with end = (last matching)
             for match-el on matching
             for match = (car match-el)
             do
             (format str "~a (~:[~a/~;~*~]~a)" (nick-of match)
                     (string= (activity-of match) "")
                     (activity-of match)
                     (describe-duration (- (expires-of match) (get-universal-time))))
             (unless (eq match-el end)
               (format str ", "))))
        (format nil "~a~@[(~a)~] is currently free" env-name status))))

(defun remove-all-env-leases (module env-name)
  (dolist (lease (leases-of module))
    (when (timer-of lease)
      (iolib:remove-timer *event-base* (timer-of lease))))
  (setf (leases-of module) (remove env-name (leases-of module) :test #'string-equal :key #'env-of)))

(defun remove-env-lease (module env-name nick)
  (let ((doomed-leases (remove-if-not (lambda (lease)
                                        (and (string-equal (env-of lease) env-name)
                                             (string-equal (nick-of lease) nick)))
                                      (leases-of module))))
    (dolist (lease doomed-leases)
      (when (timer-of lease)
        (iolib:remove-timer *event-base* (timer-of lease))))
    (setf (leases-of module) (set-difference (leases-of module) doomed-leases))))

(defun create-lease (module env-name nick time-str activity)
  (let ((expire-time (parse-expire-time time-str)))
    (cond
      ((not (member env-name (environments-of module) :test #'string-equal))
       (format nil "Can't find environment ~a" env-name))
      ((null expire-time)
       (format nil "Sorry, didn't understand ~a as an amount of time" time-str))
      (t
       (remove-env-lease module env-name nick)
       (let* ((now (get-universal-time))
              (new-lease (make-instance 'lease
                                        :nick nick
                                        :env env-name
                                        :activity activity
                                        :expires (+ now expire-time))))
         (push new-lease (leases-of module))
         (instantiate-lease-timer module new-lease now))
       (save-env-data module)
       (format nil "~a taken by ~a for ~a"
               env-name
               nick
               (describe-duration expire-time))))))

(defun env-has-lease-p (module env-name nick)
  (find-if (lambda (lease)
             (and (not (string-equal (nick-of lease) nick))
                  (string-equal (env-of lease) env-name)))
           (leases-of module)))

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
    (send "env                          - list known environments")
    (send "env <envname>                - display environment info")
    (send "env add <envname>            - add a new environment")
    (send "env remove <envname>         - remove an environment")
    (send "env help                     - display this help")))

(defmethod handle-command ((module env-module) (cmd (eql 'env))
                           message args)
  "env - manage environment leases and statuses"
  (expire-env-leases module)
  (let ((subcmd (first args)))
    (cond
      ((null args)
       (reply-to message "~:{~a~@[(~a)~]~:^, ~}"
                 (mapcar (lambda (e)
                           (list e (cdr (assoc e (statuses-of module) :test #'string-equal))))
                         (sort (environments-of module) 'string<))))
      ((string-equal subcmd "help")
       (send-env-usage message))
      ((string-equal subcmd "add")
       (cond
         ((member (second args) (environments-of module) :test #'string-equal)
          (reply-to message "That environment is already known."))
         (t
          (push (second args) (environments-of module))
          (setf (environments-of module) (sort (environments-of module) #'string<))
          (save-env-data module)
          (reply-to message "Added ~a." (second args)))))
      ((string-equal subcmd "remove")
       (cond
         ((not (member (second args) (environments-of module) :test #'string-equal))
          (reply-to message "Can't find environment ~a" (second args)))
         (t
          (setf (environments-of module) (remove (second args) (environments-of module) :test #'string-equal))
          (save-env-data module)
          (reply-to message "Removed ~a from environments." (second args)))))
      ((member subcmd (environments-of module) :test #'string-equal)
       (reply-to message (query-leases module subcmd (leases-of module))))
      (t
       (reply-to message "Can't find environment ~a" subcmd)))))

(defmethod handle-command ((module env-module) (cmd (eql 'take)) message args)
  "take <envname> <time> [<activity>] - Acquire a lease on an environment"
  (expire-env-leases module)
  (let ((lease (env-has-lease-p module (first args) (source message))))
    (if lease
        (reply-to message "~a is being leased by ~a~@[ for ~a~].  You may share or steal the environment."
                  (env-of lease)
                  (nick-of lease)
                  (activity-of lease))
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
