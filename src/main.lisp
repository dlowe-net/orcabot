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

(defvar *thread* nil)
(defvar *process-count* 1)
(defvar *quitting* nil)
(defvar *event-base* nil)

(defun session-connection-info (config)
  (let ((server (cdr (assoc 'server config)))
        (user (cdr (assoc 'user config))))
    (values
     (getf user :nickname "orca")
     (or (getf server :host)
         (error "session didn't specify a server host"))
     (getf server :port 6667)
     (getf user :username "orcabot")
     (getf user :realname "Orcabot")
     (getf server :security :none))))

(defun make-socket-and-connect (server port)
  (let ((socket (iolib:make-socket :connect :active
                                   :address-family :internet
                                   :type :stream
                                   :ipv6 t)))
    (iolib:connect socket
                   (iolib:lookup-hostname server :ipv6 t)
                   :port port :wait t)
    socket))

(defun iolib-irc-connect (&key (nickname cl-irc::*default-nickname*)
                        (username nil)
                        (realname nil)
                        (password nil)
                        (server cl-irc::*default-irc-server*)
                        (port :default)
                        (connection-type 'connection)
                        (connection-security :none)
                        (logging-stream t))
  "Create a cl-irc compatible connection object."
  (let* ((port (if (eq port :default)
                   ;; get the default port for this type of connection
                   (getf cl-irc::*default-irc-server-port* connection-security)
                   port))
         (socket (make-socket-and-connect server port))
         (stream (if (eq connection-security :ssl)
                     (cl-irc::dynfound-funcall (cl-irc::make-ssl-client-stream :cl+ssl)
                                       (iolib:socket-os-fd socket)
                                       :unwrap-stream-p nil)
                     socket))
         (connection (make-connection :connection-type connection-type
                                      :socket socket
                                      :network-stream stream
                                      :client-stream logging-stream
                                      :server-name server))
         (user (make-user connection
                          :nickname nickname
                          :username username
                          :realname realname)))
    (setf (user connection) user)
    (unless (null password)
      (pass connection password))
    (nick connection nickname)
    (user- connection (or username nickname) 0 (or realname nickname))
    (add-default-hooks connection)
    connection))

(defun orcabot-connect (config)
  (multiple-value-bind (nickname host port username realname security)
      (session-connection-info config)
    (iolib-irc-connect
     :nickname nickname
     :server host
     :username username
     :realname realname
     :password (getf (authentication-credentials host) :password)
     :port port
     :connection-security security)))

(defmethod cl-irc::irc-message-event (connection (message irc-message))
  "Redefines the standard IRC message-event handler so that it doesn't
log anything when it receives an unhandled event."
  (declare (ignore connection))
  (cl-irc::apply-to-hooks message))

(defun main-event-loop (conn)
  (iolib:set-io-handler *event-base*
                        (iolib:socket-os-fd (cl-irc::socket conn))
                        :read
                        (lambda (fd event exception)
                          (declare (ignore fd event exception))
                          (handler-case
                              (unless (cl-irc:read-message conn)
                                (iolib:exit-event-loop *event-base*))
                            (error (err)
                              (format t "Caught error ~a in cl-irc:read-message" err)))))
  (iolib:event-dispatch *event-base*))

(defun make-orcabot-instance (config)
  (lambda ()
    (let ((*quitting* nil)
          (*random-state* (make-random-state t))
          (*event-base* (make-instance 'iolib:event-base)))
      (loop until *quitting* do
           (let (conn)
             (unwind-protect
                  (handler-case
                      (progn
                        (format t "Connecting to server~%")
                        (setf conn (orcabot-connect config))
                        (initialize-access config)
                        (initialize-dispatcher conn config)
                        (handler-bind
                            ((irc:no-such-reply
                              #'(lambda (c)
                                  (declare (ignore c))
                                  (continue)))
                             (flexi-streams:external-format-encoding-error
                              #'(lambda (c)
                                  (declare (ignore c))
                                  (use-value #\?))))
                          (main-event-loop conn)))
                    (iolib:hangup (err)
                      (format t "Hangup received ~a~%" err))
                    (iolib:socket-error (err)
                      (format t "Socket error ~a~%" err))
                    (sb-int:simple-stream-error (err)
                      (format t "Simple stream error ~a~%" err))
                    (cl+ssl::ssl-error-syscall (err)
                      (format t "SSL error ~a~%" err))
                    (orcabot-exiting ()
                      (setf *quitting* t)
                      (irc:quit conn "Quitting")
                      (shutdown-dispatcher conn)
                      (setf conn nil)))
               (progn
                 (when conn
                   (shutdown-dispatcher conn)
                   (close (irc:network-stream conn) :abort t)))))
           (unless *quitting*
             (format t "Sleeping 10 seconds before reconnecting.~%")
             (sleep 10)))
      (format t "Exiting gracefully.~%"))))

(defun start-process (function name)
  "Trivial wrapper around implementation thread functions."
  (declare (ignorable name))
  #+allegro (mp:process-run-function name function)
  #+cmu (mp:make-process function :name name)
  #+lispworks (mp:process-run-function name nil function)
  #+sb-thread (sb-thread:make-thread function :name name)
  #+openmcl (ccl:process-run-function name function)
  #+armedbear (ext:make-thread function))

(defun background-orcabot-session (session-name)
  (local-time:enable-read-macros)
  (let ((config (let ((*package* (find-package "ORCABOT")))
                  (with-open-file (inf (orcabot-path "sessions/~a" session-name)
                                       :direction :input)
                    (loop for form = (read inf nil)
                       while form
                       collect form)))))
    (start-process (make-orcabot-instance config)
                   (format nil "orcabot-handler-~D" (incf *process-count*)))))

(defun start-orcabot-session (session-name)
  (local-time:enable-read-macros)
  (let ((config (let ((*package* (find-package "ORCABOT")))
                  (with-open-file (inf (orcabot-path "sessions/~a" session-name)
                                       :direction :input)
                    (loop for form = (read inf nil)
                       while form
                       collect form)))))
    (funcall (make-orcabot-instance config))))

