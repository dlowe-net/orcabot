(in-package #:orca)

(defmodule logging logging-module ()
  (channel-streams :accessor channel-streams-of :initform nil))

(defmethod initialize-module ((module logging-module)
                              config)
  (let ((log-confs (remove 'log config :test-not #'eql :key #'first)))
    (dolist (log-conf log-confs)
      (push (list (second log-conf)
                  (open (third log-conf) :direction :output
                        :if-exists :append
                        :if-does-not-exist :create))
            (channel-streams-of module)))))

(defmethod deinitialize-module ((module logging-module))
  (dolist (channel-stream (channel-streams-of module))
    (close (second channel-stream))))

(defmethod examine-message ((module logging-module) type message)
  (let ((channel-stream (assoc (first (arguments message))
                               (channel-streams-of module)
                               :test #'string=)))
    (when channel-stream
      (case type
        (irc:irc-privmsg-message
         (format (second channel-stream)
                 "~a <~a> ~a~%"
                 (local-time:format-timestring nil (local-time:now))
                 (source message)
                 (second (arguments message))))
        (irc:irc-notice-message
         (format (second channel-stream)
                 "~a -~a- ~a~%"
                 (local-time:format-timestring nil (local-time:now))
                 (source message)
                 (second (arguments message))))
        (irc:irc-kick-message
         (format (second channel-stream)
                 "~a ~a has kicked ~a~@[ (~a)~]~%"
                 (local-time:format-timestring nil (local-time:now))
                 (source message)
                 (second (arguments message))
                 (third (arguments message))))
        (irc:irc-topic-message
         (format (second channel-stream)
                 "~a ~a has set the topic to: ~a~%"
                 (local-time:format-timestring nil (local-time:now))
                 (source message)
                 (second (arguments message))))
        (irc:irc-error-message
         (format (second channel-stream)
                 "~a ERROR received: ~a~%"
                 (local-time:format-timestring nil (local-time:now))
                 (second (arguments message))))
        (irc:irc-mode-message
         (format (second channel-stream)
                 "~a ~a changed mode: ~w~%"
                 (local-time:format-timestring nil (local-time:now))
                 (source message)
                 (rest (arguments message))))
        (irc:irc-nick-message
         (format (second channel-stream)
                 "~a ~a changed nick to ~a~%"
                 (local-time:format-timestring nil (local-time:now))
                 (source message)
                 (second (arguments message))))
        (irc:irc-join-message
         (format (second channel-stream)
                 "~a ~a has joined the channel~%"
                 (local-time:format-timestring nil (local-time:now))
                 (source message)))
        (irc:irc-part-message
         (format (second channel-stream)
                 "~a ~a has left the channel~@[ (~a)~]~%"
                 (local-time:format-timestring nil (local-time:now))
                 (source message)
                 (second (arguments message))))
        (irc:irc-quit-message
         (format (second channel-stream)
                 "~a ~a has quit~@[ (~a)~]~%"
                 (local-time:format-timestring nil (local-time:now))
                 (source message)
                 (second (arguments message))))
        (irc:ctcp-action-message
         (format (second channel-stream)
                 "~a * ~a ~a~%"
                 (local-time:format-timestring nil (local-time:now))
                 (source message)

                 (subseq (second (arguments message))
                         8
                         (1- (length (second (arguments message))))))))
      (finish-output (second channel-stream)))))
