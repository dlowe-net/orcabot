(in-package #:orca)

(defvar *pounder-topic* nil)

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

(define-serious-command share (message directp env-name &rest activity-list)
  (let ((channel (first (arguments message)))
        (new-activity (when activity-list (join-string " " activity-list))))
    (cond
      ((string/= channel "#pounder")
       (reply-to message "This command only works in #pounder."))
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

(define-serious-command release (message directp env-name &rest activity-list)
  (topic-change-release message directp env-name activity-list))
(define-serious-command free (message directp env-name &rest activity-list)
  (topic-change-release message directp env-name activity-list))

(define-serious-command update (message directp env-name &rest activity-list)
  (let ((channel (first (arguments message)))
        (new-activity (when activity-list (join-string " " activity-list))))
    (cond
      ((string/= channel "#pounder")
       (reply-to message "This command only works in #pounder."))
      ((null (gethash channel *channel-topics*))
       (reply-to message "I'm not clear on the topic just yet."))
      (t
       (let* ((old-topic (gethash channel *channel-topics*))
              (users (userlist-from-topic env-name old-topic)))
         (irc:topic- *connection* channel
                     (update-topic-with-userlist env-name old-topic users
                                                 (or new-activity (activity-from-topic env-name old-topic)))))))))