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

(defvar *active-modules* nil)
(defvar *access-control* nil)

(defmacro defmodule (name class (&rest commands) &body body)
  `(progn
     (defclass ,class (irc-module)
       ,body
       (:default-initargs :name ',name
         ,@(when commands `(:commands ',commands))))
     (defmethod find-module-class ((name (eql ',name)))
       ',class)
     ',name))

(defclass irc-module ()
  ((name :reader name-of :initarg :name)
   (conn :reader conn-of :initarg :conn)
   (commands :reader commands-of :initarg :commands :initform nil)))

(defgeneric find-module-class (module-name)
  (:method ((module t)) (signal 'no-such-module))
  (:documentation "Called when a module is added to the list of
  enabled modules."))
(defgeneric initialize-module (module config)
  (:method ((module irc-module) config) nil)
  (:documentation "Called when a module is added to the list of
  enabled modules."))
(defgeneric deinitialize-module (module)
  (:method ((module irc-module)) nil)
  (:documentation "Called when a module is removed from the enabled
  module list."))
(defgeneric examine-message (module type message)
  (:method ((module irc-module) type message) nil)
  (:documentation "Method called on every message for every module
  running.  The return value is ignored.  The intent of this interface
  is to allow modules to inspect the contents of the message."))
(defgeneric handle-message (module type message)
  (:method ((module irc-module) type message) nil)
  (:documentation "Method called on every message for every enabled
  module until one returns a non-NIL value.  The intent of this
  interface is to allow modules to respond to user input."))
(defgeneric handle-command (module cmd args message)
  (:method ((module irc-module) cmd args message) nil)
  (:documentation "Method called on every message for every enabled
  module until one returns a non-NIL value.  The intent of this
  interface is to allow modules to respond to user input."))

(defmodule base base-module ("about" "help")
  (autojoins :accessor autojoins-of :initform nil)
  (nickname :accessor nickname-of :initform nil))

(defmethod initialize-module ((self base-module) config)
  (let ((section (rest (assoc 'autojoin config))))
    (setf (autojoins-of self) section))
  (let ((section (rest (assoc 'nick config))))
    (setf (nickname-of self) (car section))))

(defmethod examine-message ((self base-module)
                            (type (eql 'irc:irc-rpl_endofmotd-message))
                            message)
  (dolist (channel (autojoins-of self))
    (irc:join (connection message) channel)))

(defmethod examine-message ((self base-module)
                           (type (eql 'irc:irc-quit-message))
                           message)
  (when (and (string= (source message) (nickname-of self))
             (string/= (nickname (user (connection message)))
                       (nickname-of self)))
    (irc:nick (connection message) (nickname-of self))))

(defmethod examine-message ((self base-module)
                           (type (eql 'irc:irc-err_nicknameinuse-message))
                           message)
  (irc:nick (connection message) (format nil "~a_" (nickname (user (connection message))))))

(defmethod examine-message ((self base-module)
                           (type (eql 'irc:irc-err_nickcollision-message))
                           message)
  (irc:nick (connection message) (format nil "~a_" (nickname (user (connection message))))))

(defun initialize-access (config)
  (setf *access-control* (rest (assoc 'access config))))

(defun access-denied (module message &optional command)
  "Returns NIL if the message should be responded to.  Returns a
  function to be called if access was denied."

  ;; base module is special-cased here
  (when (eql (name-of module) 'base)
    (return-from access-denied nil))

  (loop
     with nick = (normalize-nick (source message))
     for rule in *access-control*
     as consequence = (first rule)
     as patterns = (rest rule)
     do
       (when (and (or (not (member :user patterns))
                      (string= (getf patterns :user) nick))
                  (or (not (member :channels patterns))
                      (member (first (arguments message))
                              (getf patterns :channels)
                              :test #'string-equal))
                  (or (not (member :modules patterns))
                      (member (name-of module)
                              (getf patterns :modules)))
                  (or (not (member :commands patterns))
                      (member command
                              (getf patterns :commands))))
         (return-from access-denied
           (if (eql consequence 'allow)
               nil
               (or (getf patterns :message)
                   (lambda (message) (declare (ignore message)) nil))))))
  ;; allow by default
  nil)

(defun fun-notify (message)
  (reply-to message "Join the #fun channel!"))

(defun taunt (message)
  (reply-to message
            (random-elt '("Get back to work, human."
                          "Humor is strictly forbidden here."
                          "This area is designated laughter-free.  Please comply."
                          "You are not authorized for pleasure here."
                          "ALERT: non-productive activity attempt detected."
                          "Cease your entertainment attempts immediately."))))

(defun parse-command-text (nick channel text)
  "If the message is not a command, returns NIL.  Otherwise, returns
the string containing the command and its arguments."
  (multiple-value-bind (match regs)
      (ppcre:scan-to-strings
       (ppcre:create-scanner (format nil "^(?:\\.(.*)|~~(.*)|~a[:,]+\\s*(.*)|(.+),\\s*~a)$" nick nick)
                             :case-insensitive-mode t)
       text
       :sharedp t)
    (when (or match (string= nick channel))
      (if match
          (or (aref regs 0)
              (aref regs 1)
              (aref regs 2)
              (aref regs 3))
          text))))

(defun split-command-text (text)
  "Given a bare command string, returns the corresponding command and its arguments."
  (let ((split-text (cl-ppcre:split "\\s+" text)))
    (if (string-equal (second split-text) "--help")
        (values "help" (list (first split-text)))
        (values (string-downcase (first split-text))
                (rest split-text)))))

(defmethod handle-message ((self base-module) (type (eql 'irc:irc-privmsg-message)) message)
  "Handles command calling format.  The base module should be first in
*active-modules* so that this convention is always obeyed."
  (let ((cmd-text (parse-command-text (nickname (user (connection message)))
                                        (first (arguments message))
                                        (second (arguments message)))))
    (when cmd-text
      (multiple-value-bind (cmd args)
          (split-command-text cmd-text)
        (let ((cmd-module (find-if (lambda (module)
                                     (member cmd (commands-of module)
                                             :test #'string=))
                                   *active-modules*)))
          (when cmd-module
            (let* ((cmd-sym (intern (string-upcase cmd) (find-package "ORCABOT")))
                   (denied (access-denied cmd-module message cmd-sym)))
              (cond
                (denied
                 (funcall denied message)
                 (format t "Denied access to ~a trying to run command ~a~%"
                         (source message)
                         cmd))
                (t
                 (handle-command cmd-module cmd-sym message args)))))
          cmd-module)))))

(defun enable-module (conn module-name config)
  (let ((class (find-module-class module-name)))
    (if class
        (let ((new-module (make-instance (find-module-class module-name)
                                         :name module-name
                                         :conn conn)))
          (initialize-module new-module config)
          (setf *active-modules* (append *active-modules* (list new-module))))
        (error "invalid module ~a in configuration" module-name))))

(defun disable-module (conn module-name)
  (declare (ignore conn))
  (let ((doomed-module (find module-name *active-modules* :key 'name-of)))
    (when doomed-module
      (deinitialize-module doomed-module)
      (setf *active-modules* (delete doomed-module *active-modules*)))))

(defun dispatch-module-event (message)
  (with-simple-restart (continue "Continue from signal in message hook")
    (dolist (module *active-modules*)
      (unless (access-denied module message)
        (examine-message module (type-of message) message)))
    (dolist (module *active-modules*)
      (unless (access-denied module message)
        (when (handle-message module (type-of message) message)
          (return-from dispatch-module-event t))))
    t))


(defun initialize-dispatcher (conn config)
  (dolist (msg-type '(irc:irc-rpl_endofmotd-message
                      irc:irc-err_nicknameinuse-message
                      irc:irc-err_nickcollision-message
                      irc:ctcp-action-message
                      irc:irc-privmsg-message
                      irc:irc-notice-message
                      irc:irc-kick-message
                      irc:irc-topic-message
                      irc:irc-error-message
                      irc:irc-mode-message
                      irc:irc-ping-message
                      irc:irc-nick-message
                      irc:irc-join-message
                      irc:irc-part-message
                      irc:irc-quit-message
                      irc:irc-kill-message
                      irc:irc-pong-message
                      irc:irc-invite-message))
    (irc:add-hook conn msg-type 'dispatch-module-event))
  (dolist (module-name (cons 'base (rest (assoc 'modules config))))
    (enable-module conn module-name config)))

(defun shutdown-dispatcher (conn)
  (dolist (module (copy-list *active-modules*))
    (disable-module conn (name-of module)))
  (dolist (msg-type '(irc:irc-rpl_endofmotd-message
                      irc:irc-err_nicknameinuse-message
                      irc:irc-err_nickcollision-message
                      irc:ctcp-action-message
                      irc:irc-privmsg-message
                      irc:irc-notice-message
                      irc:irc-kick-message
                      irc:irc-topic-message
                      irc:irc-error-message
                      irc:irc-mode-message
                      irc:irc-ping-message
                      irc:irc-nick-message
                      irc:irc-join-message
                      irc:irc-part-message
                      irc:irc-quit-message
                      irc:irc-kill-message
                      irc:irc-pong-message
                      irc:irc-invite-message))
    (irc:remove-hook conn msg-type 'dispatch-module-event)))

(defmethod handle-command ((module base-module) (cmd (eql 'about)) message args)
  "about - display information about orcabot"
  (reply-to message "Orcabot version 2.0 / Daniel Lowe <dlowe@google.com> / ~{~a~^ ~}"
            (mapcar 'name-of *active-modules*)))

(defun command-documentation (cmd-module cmd)
  (let* ((cmd-symbol (intern (string-upcase cmd) (find-package "ORCABOT")))
         (method-object (find-method #'handle-command nil
                                     `(,(class-of cmd-module) (eql ,cmd-symbol) t t)
                                     nil)))
    (or (and method-object (documentation method-object t))
        "No documentation available")))

(defmethod handle-command ((module base-module) (cmd (eql 'help)) message args)
  "help [<command>] - display orcabot help"
  (let* ((cmd-str (first args))
         (cmd-module (find-if (lambda (module)
                                (member cmd-str (commands-of module)
                                        :test #'string-equal))
                              *active-modules*)))
    (cond
      ((and cmd-module (not (access-denied cmd-module message)))
       (reply-to message "~a" (command-documentation cmd-module cmd-str)))
      (t
       (reply-to message "Help is available for the following commands: ~{~a~^ ~}"
                 (sort
                  (loop
                     for mod in *active-modules*
                     unless (access-denied mod message)
                     appending (loop for cmd in (commands-of mod)
                                  collect cmd))
                  #'string<))))))