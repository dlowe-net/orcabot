(in-package :orca)

(defvar *tix-cookies* (make-instance 'drakma:cookie-jar))

(defun tix-login ()
  (drakma:http-request "https:///index.html"
                       :method :post
                       :parameters '(("user" . "")
                                     ("pass" . ""))
                       :cookie-jar *tix-cookies*
                       :redirect 10))

(defun retrieve-tix-subject (tix)
  (unless (drakma:cookie-jar-cookies *tix-cookies*)
    (tix-login))
  (multiple-value-bind (response status)
      (drakma:http-request
       (format nil "https:///Ticket/Display.html?id=~a" tix)
       :cookie-jar *tix-cookies*)
    (cond
      ((/= status 200)
       nil)
      ((cl-ppcre:scan "<title>Login</title>" response)
       (tix-login)
       (retrieve-tix-subject tix))
      (t
       (let ((match (nth-value 1
                               (cl-ppcre:scan-to-strings
                                "<title>#\\d+: (.*)</title>"
                                response))))
         (when match
           (aref match 0)))))))

(defcommand tix (message directp tix)
  (multiple-value-bind (match regs)
      (ppcre:scan-to-strings "#?(\\d+)" tix)
    (cond
      ((null match)
       (reply-to message "I'd rather have a tix number."))
      (t
       (let ((subject (retrieve-tix-subject (aref regs 0))))
         (cond
           (subject
            (reply-to message
                      "tix #~a is ~a (https:///Ticket/Display.html?id=~a)"
                      (aref regs 0) subject (aref regs 0)))
           (directp
            (reply-to message "tix #~a doesn't seem to exist" (aref regs 0)))))))))