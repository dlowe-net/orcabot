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

(defmodule url url-module ("url")
  (urls :accessor urls-of :initform nil))

(defmethod initialize-module ((module url-module) config)
  (setf (urls-of module) nil)
  (with-open-file (inf (data-path "urls.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (when inf
      (setf (urls-of module) (read inf)))))

(defun save-urls (module)
  (write-to-file (data-path "urls.lisp")
                 (urls-of module)))

(defun generic-uri-summary (response headers)
  (multiple-value-bind (type subtype)
      (drakma:get-content-type headers)
    (alexandria:switch ((concatenate 'string type "/" subtype) :test #'string=)
      ("text/html"
       (let* ((dom (plump:parse response))
              (titles (and dom (plump:get-elements-by-tag-name dom "title"))))
         (when titles
           (plump:text (plump:first-child (elt titles 0))))))
      ("text/plain"
       (string-limit response 100)))))

(defun retrieve-uri-summary (uri)
  (handler-case
      (multiple-value-bind (response status headers)
          (drakma:http-request uri :want-stream t :connection-timeout 2)
        (when (= status 200)
          ;; on success read into our buffer
          (let* ((buf (make-string 8196))
                 (len (read-sequence buf response)))
            (close response)
            (generic-uri-summary (subseq buf 0 len) headers))))
    (usocket:ns-host-not-found-error ()
      "ERROR: Host not found")))

(defmethod examine-message ((module url-module)
                            (message irc:irc-privmsg-message))
  (ppcre:do-matches-as-strings  (uri-string "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)" (second (arguments message)) nil :sharedp t)
    (when (< (length uri-string) 2083)
      ;; save url
      (pushnew uri-string (urls-of module) :test #'string=)
      (save-urls module)
      ;; query url and emit summary
      (let* ((uri (puri:parse-uri uri-string))
             (summary (retrieve-uri-summary uri-string)))
        (when summary
          (reply-to message "[~a] - ~a" summary (puri:uri-host uri)))))))

(defmethod handle-command ((module url-module)
                           (cmd (eql 'url))
                           message args)
  "url [<new url>] - add a url or get a random url"
  (cond
    (args
     (push (format nil "~{~a~^ ~}" args)
           (urls-of module))
     (save-urls module)
     (reply-to message "Url added."))
    ((null (urls-of module))
     (reply-to message "There are no stored URLs."))
    (t
     (reply-to message "~a" (random-elt (urls-of module))))))
