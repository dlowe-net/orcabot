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
