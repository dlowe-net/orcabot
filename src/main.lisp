(in-package #:orca)

(defvar *thread* nil)
(defvar *nickname* "orca")
(defvar *ignored-nicks* (list *nickname* "manatee"))
(defvar *ignored-hosts* nil)
(defvar *last-said* (make-hash-table :test 'equalp))
(defvar *quiet* nil)
(defvar *admin-users* nil)
(defvar *process-count* 1)
(defvar *quitting* nil)

(defun session-connection-info (config)
  (let  ((nickname "orca")
         (username "orca")
         (realname "orcabot")
         (security :none)
         host port)
    (dolist (form config)
      (case (first form)
        (nick
         (setf nickname (second form)))
        (server
         (setf host (second form))
         (setf port (getf form :port 6667))
         (setf security (getf form :security :none)))
        (username
         (setf username (second form)))
        (realname
         (setf realname (second form)))))
    (unless (and host port security)
      (error "session didn't specify a server"))
    (unless nickname
      (error "session didn't specify a nick"))
    (values nickname host port username realname security)))

(defun orca-connect (config)
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

(defun make-orca-instance (config)
  (lambda ()
    (let ((*quitting* nil))
      (loop until *quitting* do
           (let (conn)
             (unwind-protect
                  (handler-case
                      (progn
                        (setf conn (orca-connect config))
                        (initialize-access config)
                        (dolist (module-name (cons 'base (rest (assoc 'modules config))))
                          (enable-module conn module-name config))
                        (add-module-dispatcher conn)
                        (handler-bind
                            ((irc:no-such-reply
                              #'(lambda (c)
                                  (declare (ignore c))
                                  (continue)))
                             (flexi-streams:external-format-encoding-error
                              #'(lambda (c)
                                  (declare (ignore c))
                                  (use-value #\?))))
                          (irc:read-message-loop conn)))
                    (usocket:connection-refused-error ()
                        nil)
                    (sb-int:simple-stream-error ()
                        nil)
                    (orca-exiting ()
                        (setf *quitting* t)
                        (irc:quit conn "Quitting")))
               (progn
                 (dolist (module (copy-list *orca-modules*))
                   (disable-module conn (name-of module)))
                 (remove-module-dispatcher conn)
                 (when conn
                   (close (irc:network-stream conn) :abort t)))))
           (unless *quitting*
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

(defun background-orca-session (session-name)
  (local-time:enable-read-macros)
  (let ((config (let ((*package* (find-package "ORCA")))
                  (with-open-file (inf (orca-path "sessions/~a" session-name)
                                       :direction :input)
                    (loop for form = (read inf nil)
                       while form
                       collect form)))))
    (start-process (make-orca-instance config)
                   (format nil "orca-handler-~D" (incf *process-count*)))))

(defun start-orca-session (session-name)
  (local-time:enable-read-macros)
  (let ((config (let ((*package* (find-package "ORCA")))
                  (with-open-file (inf (orca-path "sessions/~a" session-name)
                                       :direction :input)
                    (loop for form = (read inf nil)
                       while form
                       collect form)))))
    (funcall (make-orca-instance config))))

