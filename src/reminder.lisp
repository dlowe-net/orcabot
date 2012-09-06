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

(defmodule reminder reminder-module ("remind")
  (reminders :accessor reminders-of :initform nil))

(defclass reminder ()
  ((nick :accessor nick-of :initarg :nick)
   (time :accessor time-of :initarg :time)
   (message :accessor message-of :initarg :message)
   (timer :accessor timer-of :initarg :timer :initform nil)))

(defun load-reminders (module path)
  (with-open-file (inf path :if-does-not-exist nil)
    (when inf
      (loop
         with now = (get-universal-time)
         for reminder-data in (read inf nil)
         collect (apply 'make-reminder module now reminder-data)))))

(defun save-reminders (module)
  (with-open-file (ouf (orcabot-path "data/reminders.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write (mapcar
            (lambda (reminder)
              (list (nick-of reminder)
                    (time-of reminder)
                    (message-of reminder)))
            (reminders-of module))
           :stream ouf)
    (terpri ouf)))

(defmethod initialize-module ((module reminder-module) config)
  (setf (reminders-of module) (load-reminders module (orcabot-path "data/reminders.lisp"))))

(defmethod deinitialize-module ((module reminder-module))
  (mapc 'deschedule-reminder (reminders-of module)))

(defun emit-reminder (module reminder)
  (cl-irc:privmsg (conn-of module)
                  (nick-of reminder) (format nil "reminder: ~a" (message-of reminder)))
  (setf (reminders-of module)
        (delete reminder (reminders-of module)))
  (save-reminders module))

(defun make-reminder (module now nick time message)
  (let ((reminder (make-instance 'reminder
                                 :nick nick
                                 :time time
                                 :message message)))
    (prog1 reminder
      (setf (timer-of reminder)
            (iolib:add-timer *event-base*
                             (lambda ()
                               (emit-reminder module reminder))
                             (- time now)
                             :one-shot t))
      (save-reminders module))))


(defun deschedule-reminder (reminder)
  (iolib:remove-timer *event-base* (timer-of reminder)))

(defmethod handle-command ((module reminder-module)
                           (cmd (eql 'remind))
                           message args)
  "remind <time> <message> - send me a message after some time has passed"
  (cond
    ((null (cdr args))
     (reply-to message "Usage: remind <time> <message>"))
    (t
     (let ((reminder-time (parse-expire-time (first args))))
       (cond
         ((null reminder-time)
          (reply-to message "Didn't understand ~a as an amount of time" (first args)))
         (t
          (let ((now (get-universal-time)))
            (push (make-reminder module
                                 now
                                 (source message)
                                 (+ now reminder-time)
                                 (format nil "~{~a~^ ~}" (rest args)))
                  (reminders-of module)))
          (save-reminders module)
          (reply-to message "Okay, I'll remind you.")))))))
