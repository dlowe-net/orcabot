(in-package #:orca)

(define-fun-command flip (message directp)
  (if (zerop (random 2))
      (reply-to message "The coin flip is HEADS")
      (reply-to message "The coin flip is TAILS")))

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
        (irc:notice (connection message) (source message)
                    (format nil "~a ~a" (first help) (second help)))
        (irc:notice (connection message) (source message)
                    (format nil "Help is available for the following commands: ~{~a~^ ~}" (sort (mapcar #'first cmd-help) #'string<))))))

(define-fun-command action (message directp target &rest words)
  (irc::action (connection message) target (format nil "~{~a~^ ~}" words)))

(define-fun-command sayto (message directp target &rest words)
  (irc:privmsg (connection message) target (format nil "~{~a~^ ~}" words)))

(define-admin-command ignore (message directp nick)
  (let ((user (gethash nick (users (connection message)))))
    (cond
      (user
       (pushnew nick *ignored-nicks* :test #'string-equal)
       (pushnew (hostname user) *ignored-hosts* :test #'string-equal)
       (reply-to message "Ok, I'm ignoring ~a@~a now." nick (hostname user)))
      (t
       (reply-to message "Sorry, that nick doesn't exist.")))))

(define-admin-command unignore (message directp nick)
  (setf *ignored-nicks* (remove nick *ignored-nicks* :test #'string-equal))
  (reply-to message "Ok, I'm no longer ignoring ~a." nick))

(define-admin-command join (message directp &rest channels)
  (dolist (channel channels)
    (irc:join (connection message) channel)))

(define-fun-command five (message directp target)
  (let ((nick (if (string-equal target "me") (source message) target)))
    (if (char= #\# (char (first (arguments message)) 0))
        (irc::action (connection message) (first (arguments message)) (format nil "gives ~a the high five!" nick))
        (irc::action (connection message) (source message) (format nil "gives ~a the high five!" nick)))))

(define-admin-command part (message directp channel)
  (irc:part (connection message) channel))

(define-admin-command quit (message directp)
  (irc:quit (connection message) "Quitting"))

(define-admin-command nick (message directp new-nick)
  (irc:nick (connection message) new-nick))

(define-serious-command auth (message directp password)
  (cond
    ((null password)
     (reply-to message (format nil "You are~:[ not~;~] logged in as an admin"
                               (admin-user-p (source message)))))
    ((string= password (getf (authentication-credentials "orca") :password))
     (pushnew (source message) *admin-users* :test #'string=)
     (reply-to message "Hail, O mighty admin of the orca."))
    ((in-serious-channel-p message)
     (reply-to message "Access denied."))
    (t
     (reply-to message "Hah!  I fart in your general direction!"))))

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
            ((ppcre:create-scanner "^\\S+\\s+\\((\\d+)\\)\\s+- (.*)"
                                   :multi-line-mode t) output)
          (reply-to message "~a - ~a (http://linuxmanpages.com/man~a/~a.~a.php)"
                    term desc section term section)))))

(defun yodaize-sentence (sentence)
  (let* ((lc-sentence (string-downcase (string-trim " " (string-right-trim ".?!" sentence))))
         (end-punct (or (cl-ppcre:scan-to-strings "[.?!]+$" sentence) ""))
         (pivot-words '("is" "be" "will" "show" "do" "try" "are" "teach" "have"))
         (pivot (loop
                   for word in pivot-words
                   as (word-pos . word-end) = (multiple-value-list
                                               (cl-ppcre:scan (format nil "\\b~a\\b" word)
                                                              lc-sentence))
                   until word-pos
                   finally (return (when word-pos (cons word-pos word-end))))))
    (if pivot
        (string-capitalize
         (string-trim " "
                      (concatenate 'string
                                   (subseq lc-sentence (second pivot))
                                   ", "
                                   (subseq lc-sentence 0 (second pivot))
                                   end-punct))
         :end 1)
        sentence)))

(defun yodaize-string (input)
  (string-right-trim
   " "
   (with-output-to-string (response)
     (cl-ppcre:do-scans (start end reg-starts reg-ends ".*?[.?!]+" input)
       (write-string (yodaize-sentence (subseq input start end)) response)
       (write-string "  " response)))))

(define-fun-command yoda (message directp &rest input)
  (reply-to message "~a" (yodaize-string (format nil "~{~a~^ ~}" input))))