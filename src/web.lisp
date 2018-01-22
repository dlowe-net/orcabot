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
  (ignored-hosts :accessor ignored-hosts-of :initform nil)
  (urls :accessor urls-of :initform nil)
  (search-cse-id :accessor search-cse-id-of :initform nil)
  (search-api-key :accessor search-api-key-of :initform nil))

(defmethod initialize-module ((module web-module) config)
  ;; load configuration for custom search engine
  (let ((search-config (cdr (assoc 'search config))))
    (when search-config
      (setf (search-api-key-of module) (getf search-config :api-key)
            (search-cse-id-of module) (getf search-config :cse-id))))
  (let ((web-config (cdr (assoc 'web config))))
    (when web-config
      (setf (ignored-hosts-of module)
            (getf web-config :ignored-hosts))))

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

(defun find-node-by-path (root tag-names)
  "Returns the node found by traversing a DOM hierarchy by tag names."
  (let ((child-element (find (first tag-names)
                          (plump:child-elements root)
                          :key #'plump:tag-name
                          :test #'string=)))
    (cond
      ((null child-element)
       nil)
      ((endp (rest tag-names))
       child-element)
      (t
       (find-node-by-path child-element (rest tag-names))))))

(defun generic-uri-summary (response content-type)
  (handler-case
      (alexandria:switch (content-type :test #'string=)
        ("text/html"
         (let* ((text (flexi-streams:octets-to-string response :external-format :utf-8))
                (dom (plump:parse text))
                (titles (and dom (find-node-by-path dom '("html" "head" "title"))))
                (title-text-node (and titles (plump:first-child titles))))
           (when title-text-node
             (format-snippet (plump:text title-text-node)))))
        ("text/plain"
         (format-snippet (flexi-streams:octets-to-string response :external-format :utf-8))))
    (flexi-streams:external-format-error ()
      nil)))

(defun retrieve-uri-summary (uri)
  (handler-case
      (multiple-value-bind (response status headers uri)
          (drakma:http-request uri :want-stream t :connection-timeout 2
                               :cookie-jar (make-instance 'drakma:cookie-jar))
        
        (when (= status 200)
          ;; on success read into our buffer
          (let* ((buf (make-array '(32767) :element-type '(unsigned-byte 8)))
                 (uncompressed (if (string= (cdr (assoc :content-encoding headers)) "gzip")
                                   (chipz:make-decompressing-stream 'chipz:gzip response)
                                   response))
                 (len (read-sequence buf uncompressed)))
            (close response)
            (values
             (generic-uri-summary (subseq buf 0 len)
                                  (multiple-value-bind (type subtype)
                                      (drakma:get-content-type headers)
                                    (concatenate 'string type "/" subtype)))
             uri))))
    (usocket:ns-host-not-found-error ()
      "ERROR: Host not found")))

(defmethod examine-message ((module web-module)
                            (message irc:irc-privmsg-message))
  (ppcre:do-matches-as-strings  (uri-string "https?:\\/\\/[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-z]{2,6}\\b[-a-zA-Z0-9@:%_\\+.~#&//=]*\\??[-a-zA-Z0-9@:%_\\+.~#&//=]*" (second (arguments message)) nil :sharedp t)
    (when (< (length uri-string) 2083)
      (log:log-message :info "~a mentioned url ~a" (source message)
                       uri-string)
      ;; save url
      (pushnew uri-string (urls-of module) :test #'string=)
      (save-urls module)
      ;; query url and emit summary
      (handler-case
          (multiple-value-bind (summary uri)
              (retrieve-uri-summary uri-string)
            ;; TODO: maybe cache summary?
            (cond
              ((null summary)
               (log:log-message :info "No summary found for ~a" uri-string))
              ((null uri)
               (log:log-message :info "Shouldn't happen - (summary ~s) (uri ~s) (uri-string ~s)" summary uri uri-string))
              ((member (puri:uri-host uri)
                       (ignored-hosts-of module)
                       :test (lambda (a b)
                               (alexandria:ends-with-subseq b a)))
               (log:log-message :info "No summary for ignored host ~a"
                                (puri:uri-host uri)))
              (t
               (log:log-message :info "Summary for ~a : ~a" uri-string summary)
               (reply-to message "[~a] - ~a" summary (puri:uri-host uri)))))
        (puri:uri-parse-error ()
          (log:log-message :error "failed to parse url ~a" uri-string)
          nil)
        (usocket:timeout-error ()
          (log:log-message :error "socket timeout checking parse url ~a" uri-string)
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
