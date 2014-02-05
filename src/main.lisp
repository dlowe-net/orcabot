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

(defun main-event-loop (conn)
  (iolib:set-io-handler *event-base*
                        (iolib:socket-os-fd (cl-irc::socket conn))
                        :read
                        (lambda (fd event exception)
                          (declare (ignore fd event exception))
                          (cl-irc:read-message conn)))
  (iolib:event-dispatch *event-base*))

(defun orcabot-connect (config)
  (multiple-value-bind (nickname host port username realname security)
      (session-connection-info config)
    (cl-irc:connect
     :nickname nickname
     :server host
     :username username
     :realname realname
     :password (getf (authentication-credentials host) :password)
     :port port
     :connection-security security)))

(defun send-irc-keepalive (conn)
  (cond
    ((not *received-keepalive-p*)
     (error 'keepalive-failed))
    (t
     (setf *received-keepalive-p* nil)
     (cl-irc:ping conn "keepalive"))))

(defun make-orcabot-instance (config)
  (lambda ()
    (let ((*quitting* nil)
          (*random-state* (make-random-state t))
          (babel::*suppress-character-coding-errors* t)
          (*event-base* (make-instance 'iolib:event-base))
          (*received-keepalive-p* t))
      (loop until *quitting* do
           (let (conn keepalive)
             (unwind-protect
                  (handler-case
                      (progn
                        (format t "Connecting to server~%")
                        (setf conn (orcabot-connect config))
                        (format t "Initializing access~%")
                        (initialize-access config)
                        (format t "Initializing dispatcher~%")
                        (initialize-dispatcher conn config)
                        (format t "Scheduling keepalive~%")
                        (setf *received-keepalive-p* t)
                        (setf keepalive
                              (iolib:add-timer *event-base*
                                               (lambda () (send-irc-keepalive conn))
                                               60))
                        (format t "Entering main loop~%")
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
                    (keepalive-failed ()
                      (format t "Keepalive failed.  Reconnecting.~%"))
                    (orcabot-exiting ()
                      (format t "Exiting gracefully~%")
                      (setf *quitting* t)))
               (ignore-errors
                 (when keepalive
                   (iolib:remove-timer *event-base* keepalive))
                 (when conn
                   (shutdown-dispatcher conn)
                   (irc:quit conn (if *quitting*
                                      "Quitting"
                                      "Don't panic!")))))))
      (unless *quitting*
        (format t "Sleeping 10 seconds before reconnecting.~%")
        (sleep 10)))))

(defun start-process (function name)
  "Trivial wrapper around implementation thread functions."
  (declare (ignorable name))
  #+allegro (mp:process-run-function name function)
  #+cmu (mp:make-process function :name name)
  #+lispworks (mp:process-run-function name nil function)
  #+sb-thread (sb-thread:make-thread function :name name)
  #+openmcl (ccl:process-run-function name function)
  #+armedbear (ext:make-thread function))

(defun read-orcabot-config ()
  (let ((*package* (find-package "ORCABOT")))
    (with-open-file (inf (data-path "config.lisp")
                         :direction :input)
      (loop for form = (read inf nil)
         while form
         collect form))))

(defun background-orcabot-session (data-dir)
  (let ((*orcabot-data-root-pathname* data-dir))
    (local-time:enable-read-macros)
    (start-process (make-orcabot-instance (read-orcabot-config))
                   (format nil "orcabot-handler-~D" (incf *process-count*)))))

(defun start-orcabot-session (data-dir)
  (let ((*orcabot-data-root-pathname* data-dir))
    (local-time:enable-read-macros)
    (funcall (make-orcabot-instance (read-orcabot-config)))))

