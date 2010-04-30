(in-package #:orca)

(defvar *connection* nil)
(defvar *thread* nil)
(defvar *nickname* "orca")
(defvar *ignored-nicks* (list *nickname* "manatee"))
(defvar *ignored-hosts* nil)
(defvar *last-said* (make-hash-table :test 'equalp))
(defvar *quiet* nil)
(defparameter *autojoin-channels* '("#ars" "#pounder" "#restools" "#deploys"))
(defparameter *serious-channels* '("#deploys" "#orca"))

(defclass privmsg-event ()
  ((channel :accessor channel-of :initarg :channel)
   (message :accessor message-of :initarg :message)
   (nick :accessor nick-of :initarg :nick)
   (full-nick :accessor full-nick-of :initarg :nick)))

(defun shorten-nick (full-nick)
  (let ((nick (string-trim "_-[]{}" full-nick)))
    (let ((badchar-pos (or (position #\_ nick :from-end t)
                           (position #\- nick :from-end t)
                           (position #\[ nick :from-end t))))
      (when badchar-pos
        (setf nick (subseq nick 0 badchar-pos))))
    nick))

(defun make-privmsg-event (message)
  (make-instance 'privmsg-event
                 :channel (if (find (char (first (arguments message)) 0) "#+")
                              (first (arguments message))
                              (source message))
                 :full-nick (source message)
                 :message (second (arguments message))
                 :words (ppcre:split "\\s+" (second (arguments message)))
                 :nick (shorten-nick (source message))))

(define-fun-command echo (message directp &rest words)
  (reply-to message "~{~a~^ ~}" words))

(define-fun-command fail (message directp nick)
  (reply-to message "__//__//__")
  (reply-to message "\\  FAIL  /~@[  ~a is riding the fail boat!~]" nick)
  (reply-to message " \\______/ "))

(define-serious-command help (message directp command)
  (let* ((cmd-help
          '(("action" "<target> <action> - sends action to the channel or user")
            ("sayto" "<user> <action> - sends a private message to the user")
            ("join" "<channel> - make orca join a channel")
            ("part" "<channel> - make orca leave a channel")
            ("five" "<user> - make orca give the user a high five!")
            ("last" "<user> - see what the user was last seen doing")
            ("man" "<term> - look up term in unix manual")
            ("chant" "- set up a chant!")
            ("lolize" "<nick> - translate last said thing by <nick>")
            ("describe" "<term> - describe a term")
            ("remember" "<term> is <definition> - add a term")
            ("forget" "<term> - remove a term")
            ("no" "<term> is <definition> - change a term")
            ("bug" "<bug number> - show a link to the ITA bug")
            ("tix" "<tix number> - show a link to an ITA tix ticket")
            ("svn" "<revision number>/<path> - show a link to an ITA subversion revision")
            ("take" "<environment> [<activity>] - take control of pol7 or pol9")
            ("share" "<environment> [<activity>] - share pol7 or pol9 with someone else")
            ("release" "<environment> [<activity>] - stop using pol7 or pol9")
            ("update" "<environment> [<activity>] - set the activity of pol7 or pol9")))
         (help (assoc command cmd-help :test #'string-equal)))
    (if help
        (irc:notice *connection* (source message)
                    (format nil "~a ~a" (first help) (second help)))
        (irc:notice *connection* (source message)
                    (format nil "Help is available for the following commands: ~{~a~^ ~}" (sort (mapcar #'first cmd-help) #'string<))))))

(define-fun-command action (message directp target &rest words)
  (irc::action *connection* target (format nil "~{~a~^ ~}" words)))

(define-fun-command sayto (message directp target &rest words)
  (irc:privmsg *connection* target (format nil "~{~a~^ ~}" words)))

(define-admin-command ignore (message directp nick)
  (cond
    ((gethash nick (users *connection*))
     (pushnew nick *ignored-nicks* :test #'string-equal)
     (pushnew (hostname (gethash nick (users *connection*))) *ignored-hosts* :test #'string-equal)
     (reply-to message "Ok, I'm ignoring ~a now." nick))
    (t
     (reply-to message "Sorry, that nick doesn't exist."))))

(define-admin-command unignore (message directp nick)
  (setf *ignored-nicks* (remove nick *ignored-nicks* :test #'string-equal))
  (reply-to message "Ok, I'm no longer ignoring ~a." nick))

(define-admin-command join (message directp channel)
  (irc:join *connection* channel))

(define-fun-command five (message directp target)
  (let ((nick (if (string-equal target "me") (source message) target)))
    (if (char= #\# (char (first (arguments message)) 0))
        (irc::action *connection* (first (arguments message)) (format nil "gives ~a the high five!" nick))
        (irc::action *connection* (source message) (format nil "gives ~a the high five!" nick)))))

(define-admin-command part (message directp channel)
  (irc:part *connection* channel))

(define-admin-command quit (message directp)
  (irc:quit *connection* "Quitting"))

(define-serious-command last (message directp nick)
  (let ((record (first (sort (copy-seq (gethash nick *last-said*))
                             'local-time:timestamp>
                             :key #'first))))
    (cond
      ((null record)
        (reply-to message "Sorry, I haven't seen '~a'." nick))
      ((eql 'quitting (second record))
        (reply-to message "~a was last seen quitting at ~a" nick (second record)))
      ((eql 'parting (second record))
        (reply-to message "~a was last seen parting ~a at ~a"
               nick (third record) (first record)))
      ((eql 'talking (second record))
        (reply-to message "~a was last seen in ~a saying \"~a\" at ~a"
               nick (third record) (fourth record) (first record))))))

(defun thank-target (message target)
  (reply-to message (format nil "~a: ~a" target
                            (random-elt '("Thank you very much."
                                          "Thanks"
                                          "Hey, thanks"
                                          "My thanks")))))

(define-fun-command thank (message directp target)
  (thank-target message (or target (source message))))

(define-serious-command man (message directp term)
  (let ((output (with-output-to-string (str)
                  (sb-ext:run-program "/usr/bin/whatis"
                                      (list term)
                                      :input nil :output str))))
    (if (search "nothing appropriate" output)
        (reply-to message "Nothing found for ~a" term)
        (ppcre:register-groups-bind (section desc)
            ((ppcre:create-scanner "^\\S+ \\((\\d+)\\)\\s+- (.*)"
                                   :multi-line-mode t) output)
          (reply-to message "~a - ~a (http://linuxmanpages.com/man~a/~a.~a.php)"
                    term desc section term section)))))

(defun message-target-is-channel-p (message)
  (string= "#" (first (arguments message)) :end2 1))

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
                      ((ppcre:create-scanner "ticket[: #]+(\\d+)"
                                             :case-insensitive-mode t) text)
                    (list "tix" tix))
                  (ppcre:register-groups-bind (tix)
                      ((ppcre:create-scanner "tix[: #]+(\\d+)"
                                             :case-insensitive-mode t) text)
                    (list "tix" tix))
                  (ppcre:register-groups-bind (bug)
                      ((ppcre:create-scanner "bug[: #]+(\\d+)"
                                             :case-insensitive-mode t)
                       text)
                    (list "bug" bug))
                  (ppcre:register-groups-bind (rev)
                      ((ppcre:create-scanner "(?:svn [:#]*| r)(\\d+)"
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

#+nil (defun process-irc-message (message)
  (with-simple-restart (continue "Continue from signal in message hook")
    (setf *last-message* message)
    (let* ((events (mapcan (lambda (watcher)
                            (funcall watcher message)
                             watchers)))
           (actions (mapcan
                     (lambda (event)
                       (mapcan (lambda (responder event)
                                 (funcall responder event))
                               responders))
                     events)))
      (mapc 'perform-action actions))))

(defun should-be-ignored (message)
  (or
   (eql (char (second (arguments message)) 0) #\!)
   (member (source message) *ignored-nicks* :test #'string-equal)
      (let ((user (gethash (source message) (users *connection*))))
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
            (with-open-file (ouf "/home/dlowe/play/orca/data/usage.txt"
                                 :direction :output
                                 :if-exists :append
                                 :if-does-not-exist :create)
              (format ouf "[~a] <~a> ~{~a~^ ~}~%"
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
  (setf (gethash (source message) *last-said*)
        (list (append (list (local-time:now) 'quitting) (arguments message)))))

(defun part-hook (message)
  (register-last-said 'parting (source message) (arguments message)))

(defvar *channel-topics* (make-hash-table :test 'equal))

(defun topic-hook (message)
  (setf (gethash (first (arguments message)) *channel-topics*) (second (arguments message))))

(defun connected-hook (message)
  (declare (ignore message))
  (dolist (channel *autojoin-channels*)
    (irc:join *connection* channel)))

(defun error-hook (message)
  (format t "ERROR: ~s~%" message))

(defun shuffle-hooks ()
  (irc::remove-hooks *connection* 'irc::irc-privmsg-message)
  (irc::remove-hooks *connection* 'irc::irc-quit-message)
  (irc::remove-hooks *connection* 'irc::irc-part-message)
  (irc::remove-hooks *connection* 'irc::irc-topic-message)
  (irc::remove-hooks *connection* 'irc::irc-rpl_endofmotd-message)
  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
  (add-hook *connection* 'irc::irc-quit-message 'quit-hook)
  (add-hook *connection* 'irc::irc-part-message 'part-hook)
  (add-hook *connection* 'irc::irc-topic-message 'topic-hook)
  (add-hook *connection* 'irc::irc-rpl_endofmotd-message 'connected-hook)
  (add-hook *connection* 'irc::irc-err_nicknameinuse-message 'error-hook))

(defun orca-run (host)
  (local-time:enable-read-macros)
  (load-parrots)
  (load-terms)
  (load-lol-db #p"/home/dlowe/play/orca/data/lolspeak.lisp")
  (load-chat-categories #p"/home/dlowe/play/orca/data/brain.lisp")
  (setf *connection* (cl-irc:connect
                      :nickname ""
                      :server host
                      :username "orca"
                      :realname "OrcaBot 1.0d"
                      :password (getf (authentication-credentials host)
                                      :password)
                      :port 6667
                      :connection-security :ssl))
  (shuffle-hooks)
  #+(or sbcl
        openmcl)
  (setf *thread* (start-background-message-handler *connection*))
  #-(or sbcl
        openmcl)
  (read-message-loop *connection*)
  #+nil (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 8080)))
