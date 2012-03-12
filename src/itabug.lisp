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

(defmodule g itabug-module ("bug")
  (cookies :reader cookies-of :initform (make-instance 'drakma:cookie-jar)))

(defun bugzilla-login (module)
  (let ((creds (authentication-credentials "")))
    (drakma:http-request "https:///index.cgi"
                         :method :post
                         :parameters `(("Bugzilla_login" . ,(getf creds :login))
                                       ("Bugzilla_password" . ,(getf creds :password)))
                         :cookie-jar (cookies-of module))))

(defun retrieve-bug-info (module bug)
  (unless (drakma:cookie-jar-cookies (cookies-of module))
    (bugzilla-login module))
  (multiple-value-bind (response status headers)
      (drakma:http-request
       (format nil "https:///show_bug.cgi?ctype=xml&id=~a" bug)
       :cookie-jar (cookies-of module))
    (cond
      ((/= status 200)
       nil)
      ((and (string= (cdr (assoc :content-type headers))
                     "text/html; charset=UTF-8")
            (cl-ppcre:scan "need a legitimate login and password" response))
       (bugzilla-login module)
       (retrieve-bug-info module bug))
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
                (bug (elt (dom:get-elements-by-tag-name doc "bug") 0))
                (err (dom:get-attribute-node bug "error")))
           (unless err
             (values
              (dom:node-value
               (dom:first-child
                (elt (dom:get-elements-by-tag-name doc "short_desc") 0)))
              (dom:node-value
               (dom:first-child
                (elt (dom:get-elements-by-tag-name doc "assigned_to") 0)))
              (dom:node-value
               (dom:first-child
                (elt (dom:get-elements-by-tag-name doc "bug_status") 0)))))))))))

(defmethod handle-message ((module g-module)
                            (type (eql 'irc:irc-privmsg-message))
                            message)
  (let ((bugnums (all-matches-register
                  (ppcre:create-scanner "\\bbug[: #]+(\\d+)\\b" :case-insensitive-mode t)
                  (second (arguments message))
                  0
                  :sharedp t)))
    (dolist (bugnum (remove-duplicates bugnums :test #'string=))
      (multiple-value-bind (subject owner status)
          (retrieve-bug-info module bugnum)
        (when subject
          (reply-to message
                    "bug #~a is ~a [~a/~a] https:///show_bug.cgi?id=~a"
                    bugnum subject owner (string-downcase status) bugnum))))
    nil))

(defmethod handle-command ((module g-module) (cmd (eql 'bug))
                           message args)
  "bug <bug number> - show a link to an ITA bug"
  (let* ((bugnums (if (string-equal (first args) "topic")
                      (all-matches-register "bug ?(\\d{5,})"
                                            (topic (find-channel (connection message)
                                                                 (first (arguments message)))) 0
                                                                 :sharedp t)
                      args)))

    (cond
      ((null bugnums)
       (reply-to message "I'd rather have a bug number >10,000."))
      (t
       (dolist (bugnum (remove-duplicates bugnums :test #'string=))
         (multiple-value-bind (subject owner status)
             (retrieve-bug-info module bugnum)
           (cond
             (subject
              (reply-to message
                        "bug #~a is ~a [~a/~a] http:///Ticket/Display.html?id=~a"
                        bugnum subject owner status bugnum))
             (t
              (reply-to message "bug #~a doesn't seem to exist" bugnum)))))))))