(in-package :orca)

(defvar *bugzilla-cookies* (make-instance 'drakma:cookie-jar))

(defun bugzilla-login ()
  (let ((creds (authentication-credentials "")))
    (drakma:http-request "https:///index.cgi"
                         :method :post
                         :parameters `(("Bugzilla_login" . ,(getf creds :login))
                                       ("Bugzilla_password" . ,(getf creds :password)))
                         :cookie-jar *bugzilla-cookies*)))

(defun retrieve-bug-info (bug)
  (unless (drakma:cookie-jar-cookies *bugzilla-cookies*)
    (bugzilla-login))
  (multiple-value-bind (response status headers)
      (drakma:http-request
       (format nil "https:///show_bug.cgi?ctype=xml&id=~a" bug)
       :cookie-jar *bugzilla-cookies*)
    (cond
      ((/= status 200)
       nil)
      ((and (string= (cdr (assoc :content-type headers))
                     "text/html; charset=UTF-8")
            (cl-ppcre:scan "need a legitimate login and password" response))
       (bugzilla-login)
       (retrieve-bug-info bug))
      (t
       (flet ((resolver (pubid sysid)
                (declare (ignore pubid))
                (when (puri:uri= sysid
                                 (puri:parse-uri
                                  "https:///bugzilla.dtd"))
                  (open "data/bugzilla.dtd" :element-type '(unsigned-byte 8)))))
         (let* ((doc (cxml:parse response
                                 (cxml-dom:make-dom-builder)
                                 :entity-resolver #'resolver))
                (title (dom:node-value
                         (dom:first-child
                          (elt (dom:get-elements-by-tag-name doc "short_desc") 0))))
                (status (dom:node-value
                         (dom:first-child
                          (elt (dom:get-elements-by-tag-name doc "bug_status") 0))))
                (owner (dom:node-value
                         (dom:first-child
                          (elt (dom:get-elements-by-tag-name doc "assigned_to") 0)))))
           (values title owner (string-downcase status))))))))

(define-serious-command bug (message directp bug)
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
                      "bug #~a is ~a [~a/~a] https:///show_bug.cgi?id=~a"
                      (aref regs 0)
                      subject
                      owner
                      (string-downcase status)
                      (aref regs 0)))
           (directp
            (reply-to message "bug #~a doesn't seem to exist" (aref regs 0)))))))))