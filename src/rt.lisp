;;; Copyright 2012 Daniel Lowe All Rights Reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package #:orcabot)

(defmodule rt rt-module ("tix")
  (cookies :reader cookies-of :initform (make-instance 'drakma:cookie-jar))
  (base-url :accessor base-url-of))

(defun tix-login (module)
  (let ((creds (authentication-credentials (puri:uri-host (puri:uri (base-url-of module))))))
    (drakma:http-request (format nil "~a/index.html" (base-url-of module))
                         :method :post
                         :parameters `(("user" . ,(getf creds :login))
                                       ("pass" . ,(getf creds :password)))
                         :cookie-jar (cookies-of module)
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
    (tix-login module))
  (multiple-value-bind (response status)
      (drakma:http-request
       (format nil "~a/Ticket/Display.html?id=~a" (base-url-of module) tix)
       :cookie-jar (cookies-of module))
    (cond
      ((/= status 200)
       nil)
      ((cl-ppcre:scan "<title>Login</title>" response)
       (tix-login module)
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
                      "tix #~a is ~a [~a/~a] [~a/Ticket/Display.html?id=~a]"
                      tixnum subject owner status (base-url-of module) tixnum)
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
                    "tix #~a is ~a [~a/~a] ~a/Ticket/Display.html?id=~a"
                    tixnum subject owner status (base-url-of module) tixnum))))))

(defmethod initialize-module ((module rt-module) config)
  (let ((module-conf (rest (assoc 'rt config))))
    (setf (base-url-of module) (string-right-trim "/" (getf module-conf :base-url)))))

(defmethod handle-message ((module rt-module) (type (eql 'irc:irc-privmsg-message))
                           message)
  (implicit-tix-lookup module message)
  nil)

(defmethod handle-command ((module rt-module) (cmd (eql 'tix)) message args)
  "tix <tix number> - show a link to a RT ticket"
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