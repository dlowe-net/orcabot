(in-package #:orca)

(defvar *pounder-topic* nil)

(defun userlist-from-topic (env topic)
  (ppcre:register-groups-bind (userlist)
      ((if (string-equal env "pol7")
          "Pol7: ([\\w,]+)"
          "Pol9: ([\\w,]+)")
       topic)
    (unless (find userlist '("free" "idle" "busy") :test #'string-equal)
      (ppcre:split "\\s*,\\s*" userlist))))

(defun activity-from-topic (env topic)
  (ppcre:register-groups-bind (activity)
      ((if (string-equal env "pol7")
          "Pol7: [^(;|]+\\(([^)]+)\\)"
          "Pol9: [^(;|]+\\(([^)]+)\\)")
       topic)
    activity))

(defun update-topic-with-userlist (env topic userlist activity)
  (ppcre:regex-replace-all (if (string-equal env "pol7")
                               "(Pol7): [^;|]+"
                               "(Pol9): [^;|]+")
                           topic
                           (format nil "\\1: ~a~@[ (~a)~]"
                                   (cond
                                     (userlist
                                      (format nil "~{~a~^,~}" userlist))
                                     (activity
                                      "busy")
                                     (t
                                      "free"))
                                   activity)))

(defcommand take (message directp env-name &rest activity-list)
  (let ((channel (first (arguments message)))
        (new-activity (when activity-list (join-string " " activity-list))))
    (cond
      ((string/= channel "#pounder")
       (reply-to message "This command only works in #pounder."))
      ((and (string-not-equal env-name "pol7")
            (string-not-equal env-name "pol9"))
       (reply-to message "You can only take pol7 or pol9"))
      ((null (gethash channel *channel-topics*))
       (reply-to message "I'm not clear on the topic just yet."))
      (t
       (let ((old-topic (gethash channel *channel-topics*)))
         (irc:topic- *connection* channel
                     (update-topic-with-userlist env-name
                                                 old-topic
                                                 (list (shorten-nick (source message)))
                                                 (or new-activity
                                                     (activity-from-topic env-name old-topic)))))))))

(defcommand share (message directp env-name &rest activity-list)
  (let ((channel (first (arguments message)))
        (new-activity (when activity-list (join-string " " activity-list))))
    (cond
      ((string/= channel "#pounder")
       (reply-to message "This command only works in #pounder."))
      ((and (string-not-equal env-name "pol7")
            (string-not-equal env-name "pol9"))
       (reply-to message "You can only take pol7 or pol9"))
      ((null (gethash channel *channel-topics*))
       (reply-to message "I'm not clear on the topic just yet."))
      (t
       (let* ((old-topic (gethash channel *channel-topics*))
              (users (userlist-from-topic env-name old-topic)))
         (pushnew (shorten-nick (source message)) users :test #'string-equal)
         (irc:topic- *connection* channel
                     (update-topic-with-userlist env-name old-topic users
                                                 (or new-activity (activity-from-topic env-name old-topic)))))))))

(defun topic-change-release (message directp env-name activity-list)
  (let ((channel (first (arguments message)))
        (new-activity (when activity-list (join-string " " activity-list))))
    (cond
      ((string/= channel "#pounder")
       (reply-to message "This command only works in #pounder."))
      ((and (string-not-equal env-name "pol7")
            (string-not-equal env-name "pol9"))
       (reply-to message "You can only release pol7 or pol9"))
      ((null (gethash channel *channel-topics*))
       (reply-to message "I'm not clear on the topic just yet."))
      (t
       (let* ((old-topic (gethash channel *channel-topics*))
              (new-userlist (remove (shorten-nick (source message))
                                    (userlist-from-topic env-name old-topic)
                                    :test #'string-equal)))
         (irc:topic- *connection* channel
                     (update-topic-with-userlist env-name old-topic
                                                 new-userlist
                                                 (or new-activity
                                                     (and new-userlist
                                                          (activity-from-topic env-name old-topic))))))))))

(defcommand release (message directp env-name &rest activity-list)
  (topic-change-release message directp env-name activity-list))
(defcommand free (message directp env-name &rest activity-list)
  (topic-change-release message directp env-name activity-list))

(defcommand update (message directp env-name &rest activity-list)
  (let ((channel (first (arguments message)))
        (new-activity (when activity-list (join-string " " activity-list))))
    (cond
      ((string/= channel "#pounder")
       (reply-to message "This command only works in #pounder."))
      ((and (string-not-equal env-name "pol7")
            (string-not-equal env-name "pol9"))
       (reply-to message "You can only update pol7 or pol9"))
      ((null (gethash channel *channel-topics*))
       (reply-to message "I'm not clear on the topic just yet."))
      (t
       (let* ((old-topic (gethash channel *channel-topics*))
              (users (userlist-from-topic env-name old-topic)))
         (irc:topic- *connection* channel
                     (update-topic-with-userlist env-name old-topic users
                                                 (or new-activity (activity-from-topic env-name old-topic)))))))))