(in-package #:orca)

(defvar *connection* nil)
(defvar *thread* nil)
(defvar *nickname* "orca")
(defvar *ignored-nicks* (list *nickname* "manatee"))
(defvar *last-said* (make-hash-table :test 'equalp))
(defvar *autojoin-channels* '("#ars" "#pounder"))
(defvar *quiet* nil)

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
                 :words (ppcre:split "\\s+" (second (arguments message)))
                 :nick (shorten-nick (source message))))

(defcommand echo (message directp &rest words)
  (reply-to message "~{~a~^ ~}" words))

(defcommand fail (message directp nick)
  (reply-to message "__//__//__")
  (reply-to message "\\  FAIL  /~@[  ~a is riding the fail boat!~]" nick)
  (reply-to message " \\______/ "))

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
            ("tix" "<tix number> - show a link to an ITA tix ticket")
            ("svn" "<revision number> - show a link to an ITA subversion revision")
            ("thank" "- show some appreciation to your hard working bot")
            ("hi" "- greet your hard working bot")))
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
      (reply-to message "Hah!  I fart in your general direction!")))

(defcommand five (message directp target)
  (let ((nick (if (string-equal target "me") (source message) target)))
    (if (char= #\# (char (first (arguments message)) 0))
        (irc::action *connection* (first (arguments message)) (format nil "gives ~a the high five!" nick))
        (irc::action *connection* (source message) (format nil "gives ~a the high five!" nick)))))

(defcommand part (message directp channel)
  (if (string= "dlowe" (source message))
      (irc:part *connection* channel)
      (reply-to message "Go boil your bottoms, you silly English pigdogs!")))

(defcommand quit (message directp)
  (if (string= "dlowe" (source message))
      (irc:quit *connection* "Quitting")
      (reply-to message "Go boil your bottoms, you silly English pigdogs!")))

(defcommand last (message directp nick)
  (let ((record (gethash nick *last-said*)))
    (cond
      ((null record)
        (reply-to message "Sorry, I haven't seen '~a'." nick))
      ((eql 'quitting (first record))
        (reply-to message "~a was last seen quitting at ~a" nick (second record)))
      ((eql 'parting (first record))
        (reply-to message "~a was last seen parting ~a at ~a"
               nick (third record) (second record)))
      ((eql 'talking (first record))
        (reply-to message "~a was last seen in ~a saying \"~a\" at ~a"
               nick (third record) (fourth record) (second record))))))

(defun respond-to-thanks (message target)
  (reply-to message (format nil "~a: ~a" target
                            (random-elt '("You're welcome"
                                          "No problem"
                                          "Hey, I enjoy this sort of thing"
                                          "The pleasure was all mine"
                                          "No worries, mate"
                                          "Oh, it was nothing"
                                          "Let me know if there's anything else"
                                          "Don't mention it"
                                          "Anytime"
                                          "Likewise")))))

(defun thank-target (message target)
  (reply-to message (format nil "~a: ~a" target
                            (random-elt '("Thank you very much."
                                          "Thanks"
                                          "Hey, thanks"
                                          "My thanks")))))

(defun respond-to-hello (message)
  (reply-to message (format nil "~a: ~a" (source message)
                            (random-elt '("Hello"
                                  "Hey"
                                  "Hola."
                                  "Howdy"
                                  "How you doin?"
                                  "What's up?"
                                  "How's it going?"
                                  "What's happening?"
                                  "How are you?")))))

(defcommand thank (message directp target)
  (thank-target message (or target (source message))))

(defcommand thanks (message directp &rest junk)
  (declare (ignore junk))
  (respond-to-thanks message directp))

(defcommand hi (message directp &rest junk)
  (declare (ignore junk))
  (respond-to-hello message))


(defun message-target-is-channel-p (message)
  (string= "#" (first (arguments message)) :end2 1))

(defun preprocess-message (channel body)
  "Given the CHANNEL on which the message is sent, and the BODY of the message, return DIRECTP, QUESTIONP, and the preprocessed text.  DIRECTP refers to whether the bot is being directly referred to.  QUESTIONP is T when the body of the text is a ?."
  (multiple-value-bind (match regs)
      (ppcre:scan-to-strings
       (ppcre:create-scanner "^(?:~(.*)|orca[:,]+\\s*(.*)|(.+),\\s*orca)$" :case-insensitive-mode t)
       (string-trim " .?!" body)
       :sharedp t)
    (let ((text (if match
                    (or (aref regs 0)
                        (aref regs 1)
                        (aref regs 2))
                    body)))
      (values (or (equal channel *nickname*)
                  match)
              (ends-with body "?")
              (string-trim " .?!," text)))))

(defun parse-message (message)
  "Given an IRC message, returns two values.  The first value is whether or not the bot should be 'noisy'.  The second is the command to be executed by the bot.  If no command is to be executed, the function returns NIL."
  (multiple-value-bind (directp questionp text)
      (preprocess-message (first (arguments message))
                          (second (arguments message)))
    (let ((args (ppcre:split "\\s+" text)))
      (cond
        ((member (source message) *ignored-nicks* :test #'string-equal)
         nil)
        ((string= "" (first args))
         ;; empty line
         nil)
        ((starts-with text "~")
         ;; direct command
         (values t (cons (subseq (first args) 1) (rest args))))
        ((and (find (first args) '("what" "who") :test #'string-equal)
              (find (second args) '("is" "am" "are") :test #'string-equal))
         (values directp (cons "describe" (subseq args 2))))
        ((find (first args) '("what's" "who's") :test #'string-equal)
         (values directp (cons "describe" (subseq args 1))))
        ((and questionp
              (let ((term (munge-term (source message) directp text)))
                (and
                 (not (find term *ignored-terms* :test #'string-equal))
                 (or directp
                     (gethash term *terms*)))))
         (values t (cons "describe" args)))
        ((and directp
              (gethash (first args) *command-funcs*))
         (values t args))
        ((and directp
              (or (find "is" args  :test #'string-equal)
                  (find "am" args  :test #'string-equal)
                  (find "are" args  :test #'string-equal)
                  (find "means" args  :test #'string-equal)))
         (values directp (cons "remember" args)))
        (directp
         ;; an explicit mention
         (values t args))
        ((search "ticket" text :test #'char-equal)
         (ppcre:register-groups-bind (tix)
             ((ppcre:create-scanner "ticket[ #]+(\\d+)"
                                    :case-insensitive-mode t) text)
           (values directp (list "tix" tix))))
        ((search "tix" text :test #'char-equal)
         (ppcre:register-groups-bind (tix)
             ((ppcre:create-scanner "tix[ #]+(\\d+)"
                                    :case-insensitive-mode t) text)
           (values directp (list "tix" tix))))
        ((search "bug" text :test #'char-equal)
         (ppcre:register-groups-bind (bug)
             ((ppcre:create-scanner "bug[ #]+(\\d+)"
                                    :case-insensitive-mode t)
              text)
           (values directp (list "bug" bug))))
        ((search "svn" text :test #'char-equal)
         (ppcre:register-groups-bind (rev)
             ((ppcre:create-scanner "(svn |rev |revision )?[r# ]*(\\d+)"
                                    :case-insensitive-mode t)
              text)
           (values directp (list "svn" rev))))))))

(defun respond-randomly (message)
  (let ((msg (substitute #\space #\newline
                         (with-output-to-string (str)
                           (sb-ext:run-program "/usr/games/fortune"
                                               '("zippy")
                                               :input nil :output str)))))
    (if (char= #\# (char (first (arguments message)) 0))
        (irc:privmsg *connection* (first (arguments message))
                     (format nil "~a: ~a" (source message) msg))
        (irc:privmsg *connection* (source message) msg))))

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

(defun msg-hook (message)
  (with-simple-restart (continue "Continue from signal in message hook")
    (setf *last-message* message)
    (when (message-target-is-channel-p message)
      (setf (gethash (source message) *last-said*)
            (append (list 'talking (local-time:now)) (arguments message))))

    (find-chantables message)
    (parrot-learn (source message) (second (arguments message)))

    (unless *quiet*
      (multiple-value-bind (directp command)
          (parse-message message)
        (when command
          (let ((func (gethash (first command) *command-funcs*)))
            (cond
              (func
               (funcall func message directp (rest command)))
              (directp
               (respond-randomly message)))))))))

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
  (load-parrots)
  (load-terms)
#+nil  (dolist (channel *autojoin-channels*)
    (irc:join *connection* channel))
  #+(or sbcl
        openmcl)
  (setf *thread* (start-background-message-handler *connection*))
  #-(or sbcl
        openmcl)
  (read-message-loop *connection*))
