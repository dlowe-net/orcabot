(in-package #:orca)

(defmodule itatix itatix-module ("tix")
  (cookies :reader cookies-of :initform (make-instance 'drakma:cookie-jar)))

(defun tix-login (cookie-jar)
  (let ((creds (authentication-credentials "")))
    (drakma:http-request "https:///index.html"
                         :method :post
                         :parameters `(("user" . ,(getf creds :login))
                                       ("pass" . ,(getf creds :password)))
                         :cookie-jar cookie-jar
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

(defun retrieve-tix-info (module tix)
  (unless (drakma:cookie-jar-cookies (cookies-of module))
    (tix-login (cookies-of module)))
  (multiple-value-bind (response status)
      (drakma:http-request
       (format nil "https:///Ticket/Display.html?id=~a" tix)
       :cookie-jar (cookies-of module))
    (cond
      ((/= status 200)
       nil)
      ((cl-ppcre:scan "<title>Login</title>" response)
       (tix-login  (cookies-of module))
       (retrieve-tix-info module tix))
      (t
       (values
        (scrape-tix-subject response)
        (scrape-tix-owner response)
        (scrape-tix-status response))))))

(defun lookup-all-tix (module message tix-numbers)
  (let ((match-found nil))
    (dolist (tixnum (remove-duplicates tix-numbers :test #'string=))
      (multiple-value-bind (subject owner status)
          (retrieve-tix-info module tixnum)
        (cond
          (subject
            (reply-to message
                      "tix #~a is ~a [~a/~a] [http:///Ticket/Display.html?id=~a]"
                      tixnum subject owner status tixnum)
            (setf match-found t))
          (t
           (reply-to message "tix #~a doesn't seem to exist" tixnum)))))
    match-found))

(defun implicit-tix-lookup (module message)
  (let ((tixnums (all-matches-register
                  (ppcre:create-scanner "\\b(?:ticket|tix|cr)[: #]+(\\d{6,})\\b"
                                        :case-insensitive-mode t)
                  (second (arguments message))
                  0
                  :sharedp t)))
    (dolist (tixnum (remove-duplicates tixnums :test #'string=))
      (multiple-value-bind (subject owner status)
          (retrieve-tix-info module tixnum)
        (when subject
          (reply-to message
                    "tix #~a is ~a [~a/~a] http:///Ticket/Display.html?id=~a"
                    tixnum subject owner status tixnum))))))

(defmethod handle-message ((self itatix-module) (type (eql 'irc:irc-privmsg-message))
                           message)
  (implicit-tix-lookup self message)
  nil)

(defmethod handle-command ((module itatix-module) (cmd (eql 'tix)) message args)
  "tix <tix number> - show a link to an ITA tix ticket"
  (let* ((tixnums (if (string-equal (first args) "topic")
                      (all-matches-register "#?(\\d{6,})"
                                            (topic (find-channel (connection message)
                                                                 (first (arguments message)))) 0
                                                                 :sharedp t)
                      args)))

    (cond
      ((null tixnums)
       (reply-to message "I'd rather have a tix number >100,000."))
      (t
       (lookup-all-tix module message tixnums)))))