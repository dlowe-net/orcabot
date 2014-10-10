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

;;; Data in the DB module is stored in TERMS.  A term has the following slots:
;;;   KEY - the key of the term
;;;   SOURCE - the information source of the term
;;;   CONTEXT - the context of the term
;;;   PROPERTY - the attribute of the term being defined
;;;   VALUE - the value of the key's property in a given context
;;; These may be repeated if an key's property has multiple values.
;;;
;;; A query may specify any of these.  If an unspecified slot has
;;; multiple qualifiers, the query may choose to ask for disambiguation.
;;;   - Ambiguous source: "according to whom?"
;;;   - Ambiguous context: "which x do you mean?"
;;;   - Ambiguous propery: "what do you want to know about x?"
;;;
;;; Example:
;;;   ("APPEND" "CLHS" "Common Lisp" "url" "http://clhs.lisp.se/Body/f_append.htm")
;;;   ("APPEND" "Go Spec" "Go" "url" "http://golang.org/ref/spec#Appending_and_copying_slices")

;;; DB should have learned, static, and external databases.  External
;;; databases are those accessed via the web.  Static databases are
;;; simply loaded into memory at startup.  Learned databases are
;;; editable via observation and deliberate additions.
;;;
;;; Initially, we'll only have static databases, but we need to define
;;; the interfaces properly.
;;;

(defclass query ()
  ((key :accessor key-of :initarg :key)
   (source :accessor source-of :initarg :source :initform nil)
   (context :accessor context-of :initarg :context :initform nil)
   (property :accessor property-of :initarg :property :initform nil)))

(defgeneric load-database (name))
(defgeneric update-database (name))
(defgeneric database-name (db))
(defgeneric query-database (db query &key exactp))

(defclass static-database ()
  ((name :initarg :name :accessor name-of)
   terms))

(defmethod initialize-instance :after ((db static-database) &rest args)
  (declare (ignore args))
  (load-database db))

(defmethod database-name ((db static-database))
  (slot-value db 'name))

(defmethod load-database ((db static-database))
  (with-slots (name terms) db
    (with-open-file (inf (data-path (format nil "~a-db.lisp" name)) :direction :input)
      (setf terms
            (loop for term = (read inf nil)
               while term
               collect term)))))

(defmethod query-database ((db static-database) query &key exactp)
  (let ((key (string-downcase (string-trim '(#\space #\tab #\newline) (key-of query)))))
    (remove-if-not (lambda (term)
                     (and (>= (length (first term)) (length key))
                          (string-equal key (first term) :end2 (unless exactp (length key)))
                          (or (null (source-of query)) (string-equal (source-of query) (second term)))
                          (or (null (context-of query)) (string-equal (context-of query) (third term)))
                          (or (null (property-of query)) (string-equal (property-of query) (fourth term)))))
                   (slot-value db 'terms))))

;; <term>
;; <term> <property>
;; <context> <term>

(defun respond-to-query (databases query-str stream)
  (let* ((query (make-instance 'query :key query-str))
         (results (or (mapcan (lambda (db) (query-database db query :exactp t)) databases)
                      (mapcan (lambda (db) (query-database db query)) databases))))
    (cond
      ((endp results)
       ;; no results
       (format stream "No results found for ~a" query-str))
      ((endp (rest results))
       ;; one result - what we actually want
       (let ((term (first results)))
         (format stream "~a ~a: ~a"
                   (first term)
                   (fourth term)
                   (fifth term))))
      (t
       ;; more than one result
       (format stream "Which ~a do you want?" query)))))

(defmodule db db-module ()
  (databases :accessor databases-of :initform nil))

(defmethod initialize-module ((module db-module) config)
  (setf (databases-of module)
        (mapcar (lambda (db-tuple)
                  (apply #'make-instance (rest db-tuple)))
                (remove 'db config :test-not 'eql :key 'car))))

(defmethod handle-message ((module db-module)
                           (message irc:irc-privmsg-message))
  "Handle messages of the form <db> <query>
<query> can be:
  <key>
  <key> <property>
  <context> <key>
  <context> <key> <property>
"
  (ppcre:register-groups-bind (query-str)
      ("(.*)\\?\\?$" (second (arguments message)) :sharedp t)
    (reply-to message "~a"
              (with-output-to-string (result)
                (respond-to-query (databases-of module) query-str result)))
    (return-from handle-message t))

  (destructuring-bind (db-name &optional query-str)
      (re:split "\\s+" (second (arguments message)) :limit 2)
    (when (and db-name query-str)
      (let ((db (find db-name (databases-of module) :test #'string-equal :key #'name-of)))
        (when db
          (reply-to message "~a"
                    (with-output-to-string (result)
                      (respond-to-query (list db) query-str result))))))))
