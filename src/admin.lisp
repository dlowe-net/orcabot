(in-package #:orca)

(defmodule admin admin-module ("echo" "action" "sayto"
                                      "ignore" "unignore"
                                      "join" "part" "quit" "nick"))

(defmethod handle-command ((self admin-module) (cmd (eql 'quit)) message args)
  (signal 'orca-exiting))

(defmethod handle-command ((self admin-module) (cmd (eql 'echo)) message args)
  (reply-to message "~a" (second (arguments message))))

(defmethod handle-command ((self admin-module) (cmd (eql 'action)) message args)
  (irc::action (connection message)
               (first args)
               (format nil "~{~a~^ ~}" (rest args))))

(defmethod handle-command ((self admin-module) (cmd (eql 'sayto)) message args)
  (irc::privmsg (connection message)
               (first args)
               (format nil "~{~a~^ ~}" (rest args))))

(defmethod handle-command ((self admin-module) (cmd (eql 'ignore)) message args)
  (dolist (nick args)
    (pushnew (list 'deny :user nick) *access-control* :test 'string-equal))
  (if (cdr args)
      (reply-to message "Ok, I'm ignoring them.")
      (reply-to message "Ok, I'm ignoring ~a." (car args))))

(defmethod handle-command ((self admin-module) (cmd (eql 'unignore)) message args)
  (setf *access-control*
        (delete-if (lambda (nick)
                     (member nick args :test 'string-equal))
                   *access-control*))
  (if (cdr args)
      (reply-to message "Ok, I'm no longer ignoring them.")
      (reply-to message "Ok, I'm no longer ignoring ~a." (car args))))


(defmethod handle-command ((self admin-module) (cmd (eql 'join)) message args)
  (dolist (channel args)
    (irc:join (connection message) channel)))

(defmethod handle-command ((self admin-module) (cmd (eql 'part)) message args)
  (dolist (channel args)
    (irc:part (connection message) channel)))

(defmethod handle-command ((self admin-module) (cmd (eql 'nick)) message args)
  (irc:nick (connection message) (first args)))
