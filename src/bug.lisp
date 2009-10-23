(in-package :orca)

(defvar *bugzilla-cookies* (make-instance 'drakma:cookie-jar))

(defun bugzilla-login ()
  (drakma:http-request "http:///index.cgi"
                       :method :post
                       :parameters '(("Bugzilla_login" . "")
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
       nil)
      ((cl-ppcre:scan "need a legitimate login and password" response)
       (bugzilla-login)
       (retrieve-bug-title bug))
      (t
       (let ((match (nth-value 1
                               (cl-ppcre:scan-to-strings
                                "<h2>(.*)</h2>"
                                response))))
         (if match
             (html-entities:decode-entities (aref match 0))))))))

(defcommand bug (message directp bug)
  (multiple-value-bind (match regs)
      (ppcre:scan-to-strings "#?(\\d+)" bug)
    (cond
      ((null match)
       (reply-to message "I'd rather have a bug number."))
      (t
       (let ((title (retrieve-bug-title (aref regs 0))))
         (cond
           (title
            (reply-to message
                      "bug #~a is ~a (http://bug/show_bug.cgi?id=~a)"
                      (aref regs 0) title (aref regs 0)))
           (directp
            (reply-to message "bug #~a doesn't seem to exist" (aref regs 0)))))))))