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

(defmodule web web-module ("g" "url")
  (urls :accessor urls-of :initform nil)
  (search-cse-id :accessor search-cse-id-of :initform nil)
  (search-api-key :accessor search-api-key-of :initform nil))

(defmethod initialize-module ((module web-module) config)
  ;; load configuration for custom search engine
  (let ((search-config (cdr (assoc 'search config))))
    (when search-config
      (setf (search-api-key-of module) (getf search-config :api-key)
            (search-cse-id-of module) (getf search-config :cse-id))))

  (setf (urls-of module) nil)
  (with-open-file (inf (data-path "urls.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (when inf
      (setf (urls-of module) (read inf)))))

(defun save-urls (module)
  (write-to-file (data-path "urls.lisp")
                 (urls-of module)))

(defun format-snippet (snippet)
  (string-limit
   (string-trim '(#\space)
                (ppcre:regex-replace-all "\\s+" snippet " "))
   100))

(defun generic-uri-summary (response headers)
  (multiple-value-bind (type subtype)
      (drakma:get-content-type headers)
    (alexandria:switch ((concatenate 'string type "/" subtype) :test #'string=)
      ("text/html"
       (let* ((dom (plump:parse response))
              (titles (and dom (plump:get-elements-by-tag-name dom "title"))))
         (when titles
           (format-snippet (plump:text (plump:first-child (elt titles 0)))))))
      ("text/plain"
       (format-snippet response)))))

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

(defmethod examine-message ((module web-module)
                            (message irc:irc-privmsg-message))
  (ppcre:do-matches-as-strings  (uri-string "https?:\\/\\/[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b[-a-zA-Z0-9@:%_\\+.~#&//=]*\\??[-a-zA-Z0-9@:%_\\+.~#&//=]*" (second (arguments message)) nil :sharedp t)
    (when (< (length uri-string) 2083)
      ;; save url
      (pushnew uri-string (urls-of module) :test #'string=)
      (save-urls module)
      ;; query url and emit summary
      (handler-case
          (let* ((uri (puri:parse-uri uri-string))
                 (summary (retrieve-uri-summary uri-string)))
            (when summary
              (reply-to message "[~a] - ~a" summary (puri:uri-host uri))))
        (puri:uri-parse-error ()
          nil)
        (usocket:timeout-error ()
          nil)))))

(defun google-api-search (query cse-id api-key)
  (pushnew '("application" . "json") drakma:*text-content-types* :test #'equal)
  (let ((response
          (drakma:http-request "https://www.googleapis.com/customsearch/v1"
                               :parameters `(("q" . ,query)
                                             ("num" . "1")
                                             ("cx" . ,cse-id)
                                             ("key" . ,api-key)))))
    (when response
      (let* ((result (json:decode-json-from-string response))
             (item (first (cdr (assoc :items result)))))
        (and item
             (format nil "~a - ~a - ~a"
                     (cdr (assoc :link item))
                     (format-snippet (cdr (assoc :title item)))
                     (format-snippet (cdr (assoc :snippet item)))))))))

(defmethod handle-command ((module web-module)
                           (cmd (eql 'g))
                           message args)
  "g <search> - Return first Google result for search."
  (cond
    ((not (and (search-api-key-of module) (search-cse-id-of module)))
     (reply-to message "Sorry, search engine is not configured."))
    (args
     (let* ((query (join-to-string " " args))
            (result (google-api-search query
                                       (search-cse-id-of module)
                                       (search-api-key-of module))))
       (if result
           (reply-to message "~a" result)
           (reply-to message "No search results found for '~a'" query))))
    (t
     (reply-to message "Missing search terms."))))

(defmethod handle-command ((module web-module)
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
