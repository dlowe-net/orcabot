(in-package #:orca)

(defvar *connection* nil)
(defvar *thread* nil)
(defvar *nickname* "orca")
(defvar *command-funcs* (make-hash-table :test 'equalp))
(defvar *ignored-nicks* (list *nickname*))
(defvar *last-said* (make-hash-table :test 'equalp))

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
        (setf nick (subseq nick 0 (1- badchar-pos)))))
    nick))

(defun make-privmsg-event (message)
  (make-instance 'privmsg-event
                 :channel (if (find (char (first (arguments message)) 0) "#+")
                              (first (arguments message))
                              (source message))
                 :full-nick (source message)
                 :message (second (arguments message))
                 :words (cl-ppcre:split "\\s+" (second (arguments message)))
                 :nick (shorten-nick (source message))))

(defcommand echo (message directp &rest words)
  (reply "~{~a~^ ~}" words))

(defcommand fail (message directp nick)
  (reply "__//__//__")
  (reply "\\  FAIL  /~@[  ~a is riding the fail boat!~]" nick)
  (reply " \\______/ "))

(defcommand help (message directp command)
  (let* ((cmd-help
          '(("action" "<target> <action> - sends action to the channel or user")
            ("sayto" "<user> <action> - sends a private message to the user")
            ("join" "<channel> - make orca join a channel")
            ("part" "<channel> - make orca leave a channel")
            ("five" "<user> - make orca give the user a high five!")
            ("last" "<user> - see what the user was last seen doing")
            ("chant" "- set up a chant!")
            ("describe" "<term> - describe a term")
            ("remember" "<term> is <definition> - add a term")
            ("forget" "<term> - remove a term")
            ("no" "<term> is <definition> - change a term")
            ("bug" "<bug number> - show a link to the ITA bug")
            ("tix" "<tix number> - show a link to an ITA tix ticket")))
         (help (assoc command cmd-help :test #'string-equal)))
    (if help
        (irc:notice *connection* (source message)
                    (format nil "~a ~a" (first help) (second help)))
        (irc:notice *connection* (source message)
                    (format nil "Help is available for the following commands: ~{~a~^ ~}" (mapcar #'first cmd-help))))))

(defcommand action (message directp target &rest words)
  (irc::action *connection* target (format nil "~{~a~^ ~}" words)))

(defcommand sayto (message directp target &rest words)
  (irc:privmsg *connection* target (format nil "~{~a~^ ~}" words)))

(defcommand join (message directp channel)
  (if (string= "dlowe" (source message))
      (irc:join *connection* channel)
      (reply "Hah!  I fart in your general direction!")))

(defcommand five (message directp target)
  (let ((nick (if (string-equal target "me") (source message) target)))
    (if (char= #\# (char (first (arguments message)) 0))
        (irc::action *connection* (first (arguments message)) (format nil "gives ~a the high five!" nick))
        (irc::action *connection* (source message) (format nil "gives ~a the high five!" nick)))))

(defcommand part (message directp channel)
  (if (string= "dlowe" (source message))
      (irc:part *connection* channel)
      (reply "Go boil your bottoms, you silly English pigdogs!")))

(defcommand quit (message directp)
  (if (string= "dlowe" (source message))
      (irc:quit *connection* "Quitting")
      (reply "Go boil your bottoms, you silly English pigdogs!")))

(defcommand last (message directp nick)
  (let ((record (gethash nick *last-said*)))
    (cond
      ((null record)
        (reply "Sorry, I haven't seen '~a'." nick))
      ((eql 'quitting (first record))
        (reply "~a was last seen quitting at ~a" nick (second record)))
      ((eql 'parting (first record))
        (reply "~a was last seen parting ~a at ~a"
               nick (third record) (second record)))
      ((eql 'talking (first record))
        (reply "~a was last seen in ~a saying \"~a\" at ~a"
               nick (third record) (fourth record) (second record))))))

(defun message-target-is-channel-p (message)
  (string= "#" (first (arguments message)) :end2 1))

(defun preprocess-message (message)
  (let* ((channel (first (arguments message)))
         (raw-text (second (arguments message)))
         (directp (or (equal channel "orca")
                      (starts-with raw-text "~")
                      (starts-with raw-text "orca:")
                      (starts-with raw-text "orca,")))
         (questionp (ends-with raw-text "?"))
         (text (string-trim " .?!" (strip-prefixes raw-text
                                                   "orca:"
                                                   "orca,")))
         (args (cl-ppcre:split "\\s+" text)))
      (cond
        ((member (source message) *ignored-nicks* :test #'string-equal)
         nil)
        ((string= "" (first args))
         ;; empty line
         nil)
        ((starts-with text "~")
         ;; direct command
         (values t (cons (subseq (first args) 1) (rest args))))
        ((and questionp
              (or directp
                  (gethash (munge-term (source message) directp text) *terms*)))
         (values t (cons "describe" args)))
        ((and directp
              (or (find "is" args :test #'string-equal)
                  (find "am" args :test #'string-equal)
                  (find "are" args :test #'string-equal)))
         (values directp (cons "remember" args)))
        (directp
         ;; an explicit mention
         (values t args)))))

(defun respond-randomly (message)
  (declare (ignore message))
  nil)

(defvar *last-message* nil)

(defun msg-hook (message)
  (with-simple-restart (continue "Continue from signal in message hook")
    (setf *last-message* message)
    (when (message-target-is-channel-p message)
      (setf (gethash (source message) *last-said*)
            (append (list 'talking (local-time:now)) (arguments message))))

    (find-chantables message)
    (parrot-learn (source message) (second (arguments message)))

    (multiple-value-bind (directp command)
        (preprocess-message message)
      (when command
        (let ((func (gethash (first command) *command-funcs*)))
          (cond
            (func
             (funcall func message directp (rest command)))
            (directp
             (respond-randomly message))))))))

(defun quit-hook (message)
  (setf (gethash (source message) *last-said*)
        (append (list 'quitting (local-time:now)) (arguments message))))

(defun part-hook (message)
  (setf (gethash (source message) *last-said*)
        (append (list 'parting (local-time:now)) (arguments message))))

(defun shuffle-hooks ()
  (irc::remove-hooks *connection* 'irc::irc-privmsg-message)
  (irc::remove-hooks *connection* 'irc::irc-quit-message)
  (irc::remove-hooks *connection* 'irc::irc-part-message)
  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
  (add-hook *connection* 'irc::irc-quit-message 'quit-hook)
  (add-hook *connection* 'irc::irc-part-message 'part-hook))

(defun orca-run ()
  (setf *connection* (cl-irc:connect
                      :nickname ""
                      :server ""
                      :username "orca"
                      :realname "OrcaBot 1.0d"
                      :password ""
                      :port 6667
                      :ssl t))
  (shuffle-hooks)
  #+(or sbcl
        openmcl)
  (setf *thread* (start-background-message-handler *connection*))
  #-(or sbcl
        openmcl)
  (read-message-loop *connection*))
