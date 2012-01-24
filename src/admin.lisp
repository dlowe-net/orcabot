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

(defmodule admin admin-module ("echo" "action" "sayto"
                                      "ignore" "unignore"
                                      "join" "part" "quit" "nick"
                                      "eval"))

(defmethod handle-command ((self admin-module) (cmd (eql 'quit)) message args)
  "quit - make orcabot leave"
  (signal 'orcabot-exiting))

(defmethod handle-command ((self admin-module) (cmd (eql 'echo)) message args)
  "echo <stuff> - make orcabot say something"
  (when args
    (reply-to message "~{~a~^ ~}" args)))

(defmethod handle-command ((self admin-module) (cmd (eql 'action)) message args)
  "action <target> <something> - make orcabot do something to a target"
  (when (cdr args)
    (irc::action (connection message)
                 (first args)
                 (format nil "~{~a~^ ~}" (rest args)))))

(defmethod handle-command ((self admin-module) (cmd (eql 'sayto)) message args)
  "sayto <target> <something> - make orcabot say something to a target"
  (irc::privmsg (connection message)
               (first args)
               (format nil "~{~a~^ ~}" (rest args))))

(defmethod handle-command ((self admin-module) (cmd (eql 'ignore)) message args)
  "ignore <nick> - remove user from orcabot's awareness"
  (dolist (nick args)
    (pushnew (list 'deny :user nick) *access-control* :test 'string-equal))
  (if (cdr args)
      (reply-to message "Ok, I'm ignoring them.")
      (reply-to message "Ok, I'm ignoring ~a." (car args))))

(defmethod handle-command ((self admin-module) (cmd (eql 'unignore)) message args)
  "unignore <nick> - restore user to orcabot's awareness"
  (setf *access-control*
        (delete-if (lambda (nick)
                     (member nick args :test 'string-equal))
                   *access-control*))
  (if (cdr args)
      (reply-to message "Ok, I'm no longer ignoring them.")
      (reply-to message "Ok, I'm no longer ignoring ~a." (car args))))


(defmethod handle-command ((self admin-module) (cmd (eql 'join)) message args)
  "join <channel> - have orcabot join a channel"
  (dolist (channel args)
    (irc:join (connection message) channel)))

(defmethod handle-command ((self admin-module) (cmd (eql 'part)) message args)
  "part <channel> - make orcabot leave a channel"
  (dolist (channel args)
    (irc:part (connection message) channel)))

(defmethod handle-command ((self admin-module) (cmd (eql 'nick)) message args)
  "nick <channel> - make orcabot change its nick"
  (irc:nick (connection message) (first args)))

(defmethod handle-command ((self admin-module) (cmd (eql 'eval)) message args)
  "eval <expr> - evaluate an arbitrary lisp expression"
  (handler-case
      (let* ((*standard-output* (make-string-output-stream))
             (*package* (find-package "ORCABOT"))
             (expr (format nil "~{~a~^ ~}" args))
             (results (multiple-value-list (eval (read-from-string expr)))))
        (format *standard-output* "~{~s~%~}" results)
        (with-input-from-string (str (get-output-stream-string *standard-output*))
          (loop for line = (read-line str nil)
               while line
             do (reply-to message "~a" line))))
    (error (err)
        (reply-to message "ERROR: ~a~%" err))))

