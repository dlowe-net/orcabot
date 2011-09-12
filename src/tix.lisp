(in-package :orca)

(defvar *tix-cookies* (make-instance 'drakma:cookie-jar))

(defun tix-login ()
  (let ((creds (authentication-credentials "")))
    (drakma:http-request "https:///index.html"
                         :method :post
                         :parameters `(("user" . ,(getf creds :login))
                                       ("pass" . ,(getf creds :password)))
                         :cookie-jar *tix-cookies*
                         :redirect 10)))

(defun scrape-tix-subject (response)
  (let ((match (nth-value 1 (cl-ppcre:scan-to-strings
                             "<title>#\\d+: (.*)</title>"
                             response))))
    (and match (html-entities:decode-entities (aref match 0)))))

(defun scrape-tix-owner (response)
  (let ((match (nth-value 1 (cl-ppcre:scan-to-strings
                             (cl-ppcre:parse-string "<td class=\"label\">Owner:</td>\\s*<td class=\"value\">\\s*(\\S+)")
                             response))))
    (and match (html-entities:decode-entities (aref match 0)))))

(defun scrape-tix-status (response)
  (let ((match (nth-value 1 (cl-ppcre:scan-to-strings
                             "<td class=\"value status\">([^<]+)</td>"
                             response))))
    (and match (html-entities:decode-entities (aref match 0)))))

(defun retrieve-tix-info (tix)
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
       (retrieve-tix-info tix))
      (t
       (values
        (scrape-tix-subject response)
        (scrape-tix-owner response)
        (scrape-tix-status response))))))

(defun lookup-tix (message directp tix)
  (let ((match-found nil))
    (ppcre:do-register-groups (tix-num) ("#?(\\d{6,})" tix t :sharedp t)
      (setf match-found t)
      (multiple-value-bind (subject owner status)
          (retrieve-tix-info tix-num)
        (cond
          (subject
           (reply-to message
                     "tix #~a is ~a [~a/~a] (http:///Ticket/Display.html?id=~a)"
                     tix-num subject owner status tix-num))
          (directp
           (reply-to message "tix #~a doesn't seem to exist" tix-num)))))
    (unless match-found
      (reply-to message "I'd rather have a tix number >100,000."))))

(define-serious-command tix (message directp &rest tix-list)
  (dolist (tix tix-list)
    (lookup-tix message
                directp
                (if (string-equal tix "topic")
                    (topic (find-channel (connection message)
                                         (first (arguments message))))
                    tix))))

(define-serious-command ticket (message directp tix)
  (lookup-tix message directp tix))
