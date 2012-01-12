(in-package #:orca)

(defvar *orca-modules* nil)

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

(defmodule base base-module ()
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

(defvar *access-control* nil)

(defun initialize-access (config)
  (setf *access-control* (rest (assoc 'access config))))

(defun access-denied (module message &optional command)
  "Returns NIL if the message should be responded to.  Returns a
  function to be called if access was denied."

  ;; base module is special-cased here
  (when (eql (name-of module) 'base)
    (return-from access-denied nil))

  (loop
     for rule in *access-control*
     as consequence = (first rule)
     as patterns = (rest rule)
     do
       (when (and (or (not (member :user patterns))
                      (string= (getf patterns :user) (source message)))
                  (or (not (member :channels patterns))
                      (member (first (arguments message))
                              (getf patterns :channels)
                              :test #'string-equal))
                  (or (not (member :modules patterns))
                      (member (name-of module)
                              (getf patterns :modules)))
                  (or (null command)
                      (not (member :commands patterns))
                      (member command
                              (getf patterns :commands))))
         (return-from access-denied
           (if (eql consequence 'allow)
               nil
               (or (getf patterns :message)
                   (lambda (message) (declare (ignore message)) nil))))))
  ;; allow by default
  nil)

(defun taunt (message)
  (reply-to message
            (random-elt '("Get back to work, human."
                          "Humor is strictly forbidden here."
                          "This area is designated laughter-free.  Please comply."
                          "You are not authorized for pleasure here."
                          "ALERT: non-productive activity attempt detected."
                          "Cease your entertainment attempts immediately."))))

(defmethod handle-message ((self base-module) (type (eql 'irc:irc-privmsg-message)) message)
  "Handles command calling format.  The base module should be first in
*orca-modules* so that this convention is always obeyed."
  (multiple-value-bind (match regs)
      (ppcre:scan-to-strings
       (ppcre:create-scanner (format nil "^(?:~~(.*)|\\)(.*)|~a[:,]+\\s*(.*)|(.+),\\s*~a)$" (nickname (user (connection message))) (nickname (user (connection message))))
                             :case-insensitive-mode t)
       (string-trim " .?!" (remove-if-not #'graphic-char-p (second (arguments message))))
       :sharedp t)
    (when (or (equal (first (arguments message)) (nickname (user (connection message))))
              match)
      (let* ((text (string-trim " .?!,"
                                (if match
                                    (or (aref regs 0)
                                        (aref regs 1)
                                        (aref regs 2))
                                    (second (arguments message)))))
             (split-text (cl-ppcre:split "\\s+" text))
             (cmd (string-downcase (first split-text)))
             (args (rest split-text))
             (cmd-module (find-if (lambda (module)
                                    (member cmd (commands-of module)
                                            :test #'string=))
                                  *orca-modules*)))
        (when cmd-module
          (let* ((cmd-sym (intern (string-upcase cmd) (find-package "ORCA")))
                 (denied (access-denied cmd-module message cmd-sym)))
            (cond
              (denied
                (funcall denied message)
                (format t "Denied access to ~a trying to run command ~a~%"
                        (source message)
                        cmd))
              (t
                (handle-command cmd-module cmd-sym message args)))))
        ;; CMD-MODULE is NIL if the command was not found
        cmd-module))))

(defun enable-module (conn module-name config)
  (let ((new-module (make-instance (find-module-class module-name)
                                   :name module-name
                                   :conn conn)))
    (initialize-module new-module config)
    (setf *orca-modules* (append *orca-modules* (list new-module)))))

(defun disable-module (conn module-name)
  (declare (ignore conn))
  (let ((doomed-module (find module-name *orca-modules* :key 'name-of)))
    (when doomed-module
      (deinitialize-module doomed-module)
      (setf *orca-modules* (delete doomed-module *orca-modules*
                                      :key 'name-of)))))

(defun dispatch-module-event (message)
  (with-simple-restart (continue "Continue from signal in message hook")
    (dolist (module *orca-modules*)
      (unless (access-denied module message)
        (examine-message module (type-of message) message)))
    (dolist (module *orca-modules*)
      (unless (access-denied module message)
        (when (handle-message module (type-of message) message)
          (return-from dispatch-module-event t))))
    t))


(defun add-module-dispatcher (conn)
  (dolist (msg-type '(irc:irc-rpl_endofmotd-message
                      irc:irc-join-message
                      irc:irc-part-message
                      irc:irc-privmsg-message
                      irc:irc-quit-message
                      irc:irc-err_nicknameinuse-message
                      irc:irc-err_nickcollision-message))
    (irc:add-hook conn msg-type 'dispatch-module-event)))

(defun remove-module-dispatcher (conn)
  (dolist (msg-type '(irc:irc-rpl_endofmotd-message
                      irc:irc-join-message
                      irc:irc-part-message
                      irc:irc-privmsg-message
                      irc:irc-quit-message
                      irc:irc-err_nicknameinuse-message
                      irc:irc-err_nickcollision-message))
    (irc:remove-hook conn msg-type 'dispatch-module-event)))