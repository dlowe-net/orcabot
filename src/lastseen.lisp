(in-package #:orca)

(defmodule lastseen lastseen-module ("seen")
  (last-action :accessor last-action-of :initform (make-hash-table :test 'equalp)))

(defmethod initialize-module ((module lastseen-module) config)
  (clrhash (last-action-of module)))

(defun describe-message-action (message)
  (typecase message
    (irc:irc-privmsg-message
     (format nil "saying on ~a: ~a"
             (first (arguments message))
             (second (arguments message))))
    (irc:irc-notice-message
     (format nil "noting on ~a: ~a"
             (first (arguments message))
             (second (arguments message))))
    (irc:irc-kick-message
     (format nil "kicking ~a from ~a"
             (first (arguments message))
             (second (arguments message))))
    (irc:irc-topic-message
     (format nil "setting the topic on ~a to: ~a"
             (first (arguments message))
             (second (arguments message))))
    (irc:irc-mode-message
     (format nil "changing mode of ~a: ~w"
             (first (arguments message))
             (rest (arguments message))))
    (irc:irc-nick-message
     (format nil "changing nick to ~a"
             (second (arguments message))))
    (irc:irc-join-message
     (format nil "joining ~a" (first (arguments message))))
    (irc:irc-part-message
     (format nil "leaving ~a~@[ (~a)~]"
             (first (arguments message))
             (second (arguments message))))
    (irc:irc-quit-message
     (format nil
             "quitting~@[ (~a)~]"
             (second (arguments message))))
    (irc:ctcp-action-message
     (format nil
             "acting on ~a: ~a"
             (first (arguments message))
             (subseq (second (arguments message))
                     8
                     (1- (length (second (arguments message)))))))))

(defmethod examine-message ((module lastseen-module) type message)
  (setf (gethash (source message) (last-action-of module)) message))

(defmethod handle-command ((module lastseen-module)
                           (cmd (eql 'seen))
                           message args)
  (if args
      (dolist (nick args)
        (let ((last-action (gethash nick (last-action-of module))))
          (if last-action
              (reply-to message "~a was last seen at ~a ~a"
                        (source last-action)
                        (local-time:format-timestring
                         nil
                         (local-time:universal-to-timestamp
                          (received-time last-action)))
                        (describe-message-action last-action))
              (reply-to message "I haven't seen ~a" nick))))
      (reply-to message "Usage: ~seen <nick> [...]")))
