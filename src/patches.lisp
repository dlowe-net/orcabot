(in-package #:orcabot)

(defun make-socket-and-connect (server port)
  (let ((socket (iolib:make-socket :connect :active
                                   :address-family :internet
                                   :type :stream
                                   :ipv6 t)))
    (iolib:connect socket
                   (iolib:lookup-hostname server :ipv6 t)
                   :port port :wait t)
    socket))

(defun cl-irc::connect (&key (nickname cl-irc::*default-nickname*)
                          (username nil)
                          (realname nil)
                          (password nil)
                          (mode 0)
                          (server cl-irc::*default-irc-server*)
                          (port :default)
                          (connection-type 'nonblocking-connection)
                          (connection-security :none)
                          (logging-stream t))
  "Connect to server and return a connection object.

`port' and `connection-security' have a relation: when `port' equals
`:default' `*default-irc-server-port*' is used to find which port to
connect to.  `connection-security' determines which port number is found.

`connection-security' can be either `:none' or `:ssl'.  When passing
`:ssl', the cl+ssl library must have been loaded by the caller.

This version has been patched to use iolib instead of usocket.
"
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
                                      :network-stream stream
                                      :client-stream logging-stream
                                      :server-name server))
         (user (make-user connection
                          :nickname nickname
                          :username username
                          :realname realname)))
    (setf (cl-irc::socket connection) socket)
    (setf (iolib:socket-option socket :keep-alive) t)
    (setf (user connection) user)
    (unless (null password)
      (pass connection password))
    (nick connection nickname)
    (user- connection (or username nickname) mode (or realname nickname))
    (add-default-hooks connection)
    connection))

(defmethod cl-irc::default-hook ((message cl-irc::irc-rpl_topic-message))
  "Redefines the default hook for the topic reply message so that it
won't raise an error when one is received for an unknown channel"
  (destructuring-bind
        (target channel-name &optional topic)
      (arguments message)
    (declare (ignore target))
    (let* ((connection (cl-irc::connection message))
           (channel (cl-irc::find-channel connection channel-name)))
      (when channel
        (setf (cl-irc::topic channel) topic)))))

(defmethod cl-irc::irc-message-event (connection (message cl-irc::irc-message))
  "Redefines the standard IRC message-event handler so that it doesn't
log anything when it receives an unhandled event."
  (declare (ignore connection))
  (cl-irc::apply-to-hooks message))

(defclass nonblocking-connection (cl-irc::connection)
  ((cl-irc::socket :accessor cl-irc::socket :initform nil)
   (line-buffer :accessor line-buffer :initform (make-array '(0)
                                                             :adjustable t
                                                             :fill-pointer 0))))

(defun cl-irc::read-protocol-line (connection)
  "Reads a line from the input network stream, returning a
character array with the input read."
  (multiple-value-bind (buf buf-len incompletep)
      (cl-irc::read-sequence-until (network-stream connection)
                                   (make-array 1024
                                               :element-type '(unsigned-byte 8)
                                               :fill-pointer t)
                                   '(10)
                                   :non-blocking t)
    (let ((line-buffer (line-buffer connection)))
      (loop
         for c across buf
         for i upto (1- buf-len)
         do (vector-push-extend c (line-buffer connection)))

      (unless incompletep
        (setf (fill-pointer line-buffer)
              ;; remove all trailing CR and LF characters
              ;; (This allows non-conforming clients to send CRCRLF
              ;;  as a line separator too).
              (or (position-if #'(lambda (x) (member x '(10 13)))
                               line-buffer :from-end t :end (fill-pointer line-buffer))
                  (fill-pointer line-buffer)))
        (prog1
            (cl-irc::try-decode-line buf cl-irc::*default-incoming-external-formats*)
          ;; Reset line-buffer once the line is decoded
          (setf (fill-pointer line-buffer) 0))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *new-reply-names*
    '((900 :rpl_loggedin)
      (901 :rpl_loggedout)
      (902 :err_nicklocked)
      (903 :rpl_saslsuccess)
      (904 :err_saslfail)
      (905 :err_sasltoolong)
      (906 :err_saslaborted)
      (907 :err_saslalready)
      (908 :rpl_saslmechs)
      (524 :err_helpnotfound)
      (525 :err_invalidkey)
      (670 :rpl_starttls)
      (671 :rpl_whoissecure)
      (691 :err_starttls)
      (696 :err_invalidmodeparam)
      (704 :rpl_helpstart)
      (705 :rpl_helptxt)
      (706 :rpl_endofhelp)
      (723 :err_noprivs)
      )))


(in-package #:cl-irc)

(setf cl-irc::*reply-names* (append cl-irc::*reply-names* orcabot::*new-reply-names*))
(cl-irc::create-irc-message-classes #.(mapcar #'second orcabot::*new-reply-names*))
