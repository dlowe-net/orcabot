(in-package :orca)

(defvar *tix-cookies* (make-instance 'drakma:cookie-jar))

(defun tix-login ()
  (drakma:http-request "https:///index.html"
                       :method :post
                       :parameters '(("user" . "dlowe")
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
       "<tix not found >")
      ((cl-ppcre:scan "<title>Login</title>" response)
       (tix-login)
       (retrieve-tix-subject tix))
      (t
       (let ((match (nth-value 1
                               (cl-ppcre:scan-to-strings
                                "<title>#\\d+: (.*)</title>"
                                response))))
         (if match
             (aref match 0)
             "<tix not found>"))))))

(defcommand tix (message directp tix)
  (if (every #'digit-char-p tix)
      (reply "~a - https:///Ticket/Display.html?id=~a"
             (retrieve-tix-subject tix) tix)
      (reply "I'd rather have a tix number.")))