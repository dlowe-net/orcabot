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

(defun userlist-from-topic (env topic)
  (let ((regex (ppcre:create-scanner (format nil "~a: ([\\w,]+)" env)
                                     :case-insensitive-mode t)))
    (ppcre:register-groups-bind (userlist)
        (regex topic)
      (unless (find userlist '("free" "idle" "busy") :test #'string-equal)
        (ppcre:split "\\s*,\\s*" userlist)))))

(defun activity-from-topic (env topic)
  (let ((regex (ppcre:create-scanner (format nil "~a: [^(;|]+\\(([^)]+)\\)"
                                             env)
                                     :case-insensitive-mode t)))
    (ppcre:register-groups-bind (activity)
        (regex topic)
      activity)))

(defun update-topic-with-userlist (env topic userlist activity)
  (let ((regex (ppcre:create-scanner (format nil "(~a): [^;|]+" env)
                                     :case-insensitive-mode t)))
    (ppcre:regex-replace-all regex topic
                             (format nil "\\1: ~a~@[ (~a)~]"
                                     (cond
                                       (userlist
                                        (format nil "~{~a~^,~}" userlist))
                                       (activity
                                        "busy")
                                       (t
                                        "free"))
                                     activity))))

(define-serious-command take (message directp env-name &rest activity-list)
  (let ((channel (first (arguments message)))
        (new-activity (when activity-list (join-string " " activity-list))))
    (cond
      ((string/= channel "#pounder")
       (reply-to message "This command only works in #pounder."))
      (t
       (let ((old-topic (topic (find-channel (connection message) channel))))
         (irc:topic- (connection message) channel
                     (update-topic-with-userlist env-name
                                                 old-topic
                                                 (list (shorten-nick (source message)))
                                                 (or new-activity
                                                     (activity-from-topic env-name old-topic)))))))))

(define-serious-command share (message directp env-name &rest activity-list)
  (let ((channel (first (arguments message)))
        (new-activity (when activity-list (join-string " " activity-list))))
    (cond
      ((string/= channel "#pounder")
       (reply-to message "This command only works in #pounder."))
      (t
       (let* ((old-topic (gethash channel *channel-topics*))
              (users (userlist-from-topic env-name old-topic)))
         (pushnew (shorten-nick (source message)) users :test #'string-equal)
         (irc:topic- (connection message) channel
                     (update-topic-with-userlist env-name old-topic users
                                                 (or new-activity (activity-from-topic env-name old-topic)))))))))

(defun topic-change-release (message env-name activity-list)
  (let ((channel (first (arguments message)))
        (new-activity (when activity-list (join-string " " activity-list))))
    (cond
      ((string/= channel "#pounder")
       (reply-to message "This command only works in #pounder."))
      (t
       (let* ((old-topic (topic (find-channel (connection message) channel)))
              (new-userlist (remove (shorten-nick (source message))
                                    (userlist-from-topic env-name old-topic)
                                    :test #'string-equal)))
         (irc:topic- (connection message) channel
                     (update-topic-with-userlist env-name old-topic
                                                 new-userlist
                                                 (or new-activity
                                                     (and new-userlist
                                                          (activity-from-topic env-name old-topic))))))))))

(define-serious-command release (message directp env-name &rest activity-list)
  (topic-change-release message env-name activity-list))
(define-serious-command free (message directp env-name &rest activity-list)
  (topic-change-release message env-name activity-list))

(define-serious-command update (message directp env-name &rest activity-list)
  (let ((channel (first (arguments message)))
        (new-activity (when activity-list (join-string " " activity-list))))
    (cond
      ((string/= channel "#pounder")
       (reply-to message "This command only works in #pounder."))
      (t
       (let* ((old-topic (topic (find-channel (connection message) channel)))
              (users (userlist-from-topic env-name old-topic)))
         (irc:topic- (connection message) channel
                     (update-topic-with-userlist env-name old-topic users
                                                 (or new-activity (activity-from-topic env-name old-topic)))))))))