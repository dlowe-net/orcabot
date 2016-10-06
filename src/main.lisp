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
  (let ((fd (iolib:socket-os-fd (cl-irc::socket conn))))
    (unwind-protect
         (progn
           (iolib:set-io-handler *event-base*
                                 fd
                                 :read
                                 (lambda (fd event exception)
                                   (declare (ignore fd event exception))
                                   (cl-irc:read-message conn)))
           (iolib:event-dispatch *event-base*))
      (when (iolib.multiplex::fd-monitored-p *event-base* fd :read)
        (iolib:remove-fd-handlers *event-base* fd)))))

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

(defun make-orcabot-instance (data-dir &key log-to-stdoutp)
  (lambda ()
    (let* ((*quitting* nil)
           (*random-state* (make-random-state t))
           (babel::*suppress-character-coding-errors* t)
           (*event-base* (make-instance 'iolib:event-base))
           (*received-keepalive-p* t)
           (*orcabot-data-root-pathname* data-dir)
           (config (read-orcabot-config)))
      (local-time:enable-read-macros)
      (setf (log:log-manager)
            (make-instance 'log:log-manager :message-class 'log:formatted-message))
      (when log-to-stdoutp
        (log:start-messenger 'log:text-stream-messenger
                             :name 'stdout
                             :stream *standard-output*))
      (log:start-messenger 'log:text-file-messenger
                           :name 'tofile
                           :filename (data-path "orcabot.log"))
      (log:log-message :notice "Starting orcabot...")
      (loop until *quitting* do
           (let (conn keepalive)
             (unwind-protect
                  (handler-case
                      (progn
                        (log:log-message :info "Connecting to server")
                        (setf conn (orcabot-connect config))
                        (log:log-message :info "Initializing dispatcher")
                        (initialize-dispatcher conn config)
                        (log:log-message :info "Scheduling keepalive")
                        (setf *received-keepalive-p* t)
                        (setf keepalive
                              (iolib:add-timer *event-base*
                                               (lambda () (send-irc-keepalive conn))
                                               60))
                        (log:log-message :info "Entering main loop")
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
                      (log:log-message :notice "Hangup received ~a" err))
                    (iolib:socket-error (err)
                      (log:log-message :error "Socket error ~a" err))
                    (sb-int:simple-stream-error (err)
                      (log:log-message :error "Simple stream error ~a" err))
                    (cl+ssl::ssl-error-syscall (err)
                      (log:log-message :error "SSL error ~a" err))
                    (keepalive-failed ()
                      (log:log-message :error "Keepalive failed.  Reconnecting."))
                    (orcabot-exiting ()
                      (log:log-message :info "Exiting gracefully")
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
        (log:log-message :info "Sleeping 10 seconds before reconnecting.")
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
    (local-time:enable-read-macros)
    (with-open-file (inf (data-path "config.lisp")
                         :direction :input)
      (loop for form = (read inf nil)
         while form
         collect form))))

(defun background-orcabot-session (data-dir)
  (start-process (make-orcabot-instance data-dir)
                 (format nil "orcabot-handler-~D" (incf *process-count*))))

(defun start-orcabot-session (data-dir)
  (funcall (make-orcabot-instance data-dir :log-to-stdoutp t)))

