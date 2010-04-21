(in-package :orca)

(defvar *bugzilla-cookies* (make-instance 'drakma:cookie-jar))

(defun bugzilla-login ()
  (let ((creds (authentication-credentials "")))
    (drakma:http-request "http:///index.cgi"
                         :method :post
                         :parameters `(("Bugzilla_login" . ,(getf creds :login))
                                       ("Bugzilla_password" . ,(getf creds :password)))
                         :cookie-jar *bugzilla-cookies*)))

(defun scrape-bug-title (response)
  (let ((match (nth-value 1 (cl-ppcre:scan-to-strings
                             "<h2>(.*)</h2>"
                             response))))
    (and match (html-entities:decode-entities (aref match 0)))))

(defun scrape-bug-owner (response)
  (let ((match (nth-value 1 (cl-ppcre:scan-to-strings
                             "Assigned&nbsp;To</a>:\\s*</b>\\s*</td>\\s*<td>[^&]+&lt;([^&]+)&gt;</td>"
                             response))))
    (and match (html-entities:decode-entities (aref match 0)))))

(defun scrape-bug-status (response)
  (let ((match (nth-value 1 (cl-ppcre:scan-to-strings
                             "Status</a>:\\s*</b>\\s*</td>\\s*<td>([^<]+)</td>"
                             response))))
    (and match (html-entities:decode-entities (aref match 0)))))

(defun retrieve-bug-info (bug)
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
       (retrieve-bug-info bug))
      (t
       (values
        (scrape-bug-title response)
        (scrape-bug-owner response)
        (scrape-bug-status response))))))

(defcommand bug (message directp bug)
  (multiple-value-bind (match regs)
      (ppcre:scan-to-strings "#?(\\d+)" bug)
    (cond
      ((null match)
       (reply-to message "I'd rather have a bug number."))
      (t
       (multiple-value-bind (subject owner status)
           (retrieve-bug-info (aref regs 0))
         (cond
           (subject
            (reply-to message
                      "bug #~a is ~a [~a/~a] (http://bug/show_bug.cgi?id=~a)"
                      (aref regs 0)
                      subject
                      owner
                      (string-downcase status)
                      (aref regs 0)))
           (directp
            (reply-to message "bug #~a doesn't seem to exist" (aref regs 0)))))))))