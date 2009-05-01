(in-package :orca)

(defvar *bugzilla-cookies* (make-instance 'drakma:cookie-jar))

(defun bugzilla-login ()
  (drakma:http-request "http:///index.cgi"
                       :method :post
                       :parameters '(("Bugzilla_login" . "dlowe")
                                     ("Bugzilla_password" . ""))
                       :cookie-jar *bugzilla-cookies*))

(defun retrieve-bug-title (bug)
  (unless (drakma:cookie-jar-cookies *bugzilla-cookies*)
    (bugzilla-login))
  (multiple-value-bind (response status)
      (drakma:http-request
       (format nil "http:///show_bug.cgi?id=~a" bug)
       :cookie-jar *bugzilla-cookies*)
    (cond
      ((/= status 200)
       "<bug not found>")
      ((cl-ppcre:scan "need a legitimate login and password" response)
       (bugzilla-login)
       (retrieve-bug-title bug))
      (t
       (let ((match (nth-value 1
                               (cl-ppcre:scan-to-strings
                                "<h2>(.*)</h2>"
                                response))))
         (if match
             (aref match 0)
             "<bug not found>"))))))

(defcommand bug (message directp bug)
  (if (every #'digit-char-p bug)
      (reply "~a - http:///show_bug.cgi?id=~a"
             (retrieve-bug-title bug) bug)
      (reply "I'd rather have a bug number.")))