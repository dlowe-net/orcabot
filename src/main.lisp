(in-package #:orca)

(defvar *thread* nil)
(defvar *nickname* "orca")
(defvar *ignored-nicks* (list *nickname* "manatee"))
(defvar *ignored-hosts* nil)
(defvar *last-said* (make-hash-table :test 'equalp))
(defvar *quiet* nil)
(defvar *admin-users* nil)
(defvar *process-count* 1)

(defparameter *autojoin-channels* '("#ars" "#pounder" "#restools" "#deploys" "#magic"))
(defparameter *serious-channels* '("#deploys"))

(defun preprocess-message (channel body)
  "Given the CHANNEL on which the message is sent, and the BODY of the message, return DIRECTP, and the preprocessed text.  DIRECTP refers to whether the bot is being directly referred to."
  (multiple-value-bind (match regs)
      (ppcre:scan-to-strings
       (ppcre:create-scanner "^(?:~(.*)|\\)(.*)|orca[:,]+\\s*(.*)|(.+),\\s*orca)$" :case-insensitive-mode t)
       (string-trim " .?!" (remove-if-not #'graphic-char-p body))
       :sharedp t)
    (let ((text (if match
                    (or (aref regs 0)
                        (aref regs 1)
                        (aref regs 2))
                    body)))
      (values (or (equal channel *nickname*)
                  match)
              (string-trim " .?!," text)))))

(defun parse-message (message)
  "Given an IRC message, returns two values.  The first value is whether or not the bot should be 'noisy'.  The second is the command to be executed by the bot.  If no command is to be executed, the function returns NIL."
  (multiple-value-bind (directp text)
      (preprocess-message (first (arguments message))
                          (second (arguments message)))
    (let ((args (ppcre:split "\\s+" text)))
      (cond
        ((string= "" (first args))
         ;; empty line
         nil)
        ((and directp
              (gethash (first args) *command-funcs*))
         (values t args))
        (directp
         ;; an explicit mention
         (values t args))
        (t
         (values directp
                 (or
                  (ppcre:register-groups-bind (tix)
                      ((ppcre:create-scanner "\\bticket[: #]+(\\d+)\\b"
                                             :case-insensitive-mode t) text)
                    (list "tix" tix))
                  (ppcre:register-groups-bind (tix)
                      ((ppcre:create-scanner "\\btix[: #]+(\\d+)\\b"
                                             :case-insensitive-mode t) text)
                    (list "tix" tix))
                  (ppcre:register-groups-bind (bug)
                      ((ppcre:create-scanner "\\bbug[: #]+(\\d+)\\b"
                                             :case-insensitive-mode t)
                       text)
                    (list "bug" bug))
                  (ppcre:register-groups-bind (rev)
                      ((ppcre:create-scanner "\\b(?:svn [:#]*|r)(\\d{6,})(?:$|[^\\d-])"
                                             :case-insensitive-mode t)
                       text)
                    (list "svn" rev))
                  nil)))))))

(defun in-serious-channel-p (message)
  (find (first (arguments message)) *serious-channels* :test #'string-equal))

(defun respond-randomly (message)
  (if (in-serious-channel-p message)
      (reply-to message "Get back to work, human.")
      (chat message)))

(defvar *last-message* nil)

(defun should-be-ignored (message)
  (or
   (eql (char (second (arguments message)) 0) #\!)
   (member (source message) *ignored-nicks* :test #'string-equal)
      (let ((user (gethash (source message) (users (connection message)))))
        (and user (member (hostname user) *ignored-hosts* :test #'string-equal)))))

(defun register-last-said (action source arguments)
  (setf (gethash source *last-said*)
        (cons (append (list (local-time:now) action) arguments)
              (delete (first arguments)
                      (gethash source *last-said*)
                      :test #'string=
                      :key #'third))))

(defun msg-hook (message)
  (with-simple-restart (continue "Continue from signal in message hook")
    (unless (should-be-ignored message)
      (setf *last-message* message)
      (register-last-said 'talking (source message) (arguments message))
      (find-chantables message)
      (parrot-learn (source message) (second (arguments message)))

      (unless *quiet*
        (multiple-value-bind (directp command)
            (parse-message message)
          (when directp
            (with-open-file (ouf (orca-path "data/usage.txt")
                                 :direction :output
                                 :if-exists :append
                                 :if-does-not-exist :create)
              (format ouf "~a [~a] <~a> ~{~a~^ ~}~%"
                      (local-time:now)
                      (first (arguments message))
                      (source message)
                      (rest (arguments message)))))
          (when command
            (let ((func (gethash (first command) *command-funcs*)))
              (cond
                (func
                 (funcall func message directp (rest command)))
                (directp
                 (respond-randomly message))))))))))

(defun quit-hook (message)
  ;; Recover own nick if someone else had it
  (when (and (string= (source message) *nickname*)
             (string/= (nickname (user (connection message)))
                       *nickname*))
    (irc:nick (connection message) *nickname*))
  ;; Remove user from admin list
  (setf *admin-users* (delete (source message) *admin-users* :test #'string=))
  ;; Add to last seen
  (setf (gethash (source message) *last-said*)
        (list (append (list (local-time:now) 'quitting) (arguments message)))))

(defun part-hook (message)
  (register-last-said 'parting (source message) (arguments message)))

(defun connected-hook (message)
  (dolist (channel *autojoin-channels*)
    (irc:join (connection message) channel)))

(defun nickname-hook (message)
  (irc:nick (connection message) (format nil "~a_" *nickname*)))

(defun shuffle-hooks (conn)
  (irc::remove-hooks conn 'irc::irc-privmsg-message)
  (irc::remove-hooks conn 'irc::irc-quit-message)
  (irc::remove-hooks conn 'irc::irc-part-message)
  (irc::remove-hooks conn 'irc::irc-rpl_endofmotd-message)
  (add-hook conn 'irc::irc-privmsg-message 'msg-hook)
  (add-hook conn 'irc::irc-quit-message 'irc::default-hook)
  (add-hook conn 'irc::irc-part-message 'irc::default-hook)
  (add-hook conn 'irc::irc-quit-message 'quit-hook)
  (add-hook conn 'irc::irc-part-message 'part-hook)
  (add-hook conn 'irc::irc-rpl_endofmotd-message 'connected-hook)
  (add-hook conn 'irc::irc-err_nicknameinuse-message 'nickname-hook)
  (add-hook conn 'irc::irc-err_nickcollision-message 'nickname-hook))

(defun make-orca-instance (nickname host port username realname security)
  (lambda ()
    (loop
       (handler-case
           (let ((conn (cl-irc:connect
                        :nickname nickname
                        :server host
                        :username username
                        :realname realname
                        :password (getf (authentication-credentials host) :password)
                        :port port
                        :connection-security security)))
             (shuffle-hooks conn)
             (handler-bind
                 ((irc:no-such-reply
                   #'(lambda (c)
                       (declare (ignore c))
                       (continue)))
                  (flexi-streams:external-format-encoding-error
                   #'(lambda (c)
                       (declare (ignore c))
                       (use-value #\?))))
               (irc:read-message-loop conn))
             (close (irc:network-stream conn) :abort t))
         (usocket:connection-refused-error
             nil))
       (sleep 10))))

(defun start-process (function name)
  "Trivial wrapper around implementation thread functions."
  (declare (ignorable name))
  #+allegro (mp:process-run-function name function)
  #+cmu (mp:make-process function :name name)
  #+lispworks (mp:process-run-function name nil function)
  #+sb-thread (sb-thread:make-thread function :name name)
  #+openmcl (ccl:process-run-function name function)
  #+armedbear (ext:make-thread function))

(defun orca-run (nickname host
                 &key
                 (port 6667)
                 (username nickname)
                 (realname "OrcaBot 1.0d")
                 (security :none))
  (local-time:enable-read-macros)
  (load-parrots)
  (load-terms)
  (load-lol-db (orca-path "data/lolspeak.lisp"))
  (load-chat-categories (orca-path "data/brain.lisp"))
  (load-tournament)
  (start-process (make-orca-instance nickname host port username realname security)
                 (format nil "orca-handler-~D" (incf *process-count*)))
  #+nil (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 8080)))
