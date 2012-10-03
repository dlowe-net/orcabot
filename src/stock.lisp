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

(defmodule stock stock-module ("stock"))

(defun valid-stock-symbol-p (str)
  (every (lambda (c)
           (or (alpha-char-p c)
               (eql c #\^))) str))

(defun retrieve-stock-quotes (symbols)
  (let ((url (format nil "http://finance.yahoo.com/d/quotes.csv?s=~{~a~^+~}&f=d1snl1p2j1"
                     (mapcar (lambda (s)
                               (drakma::url-encode s drakma:*drakma-default-external-format*))
                             (mapcar 'string-upcase symbols)))))
    (multiple-value-bind (response status)
        (drakma:http-request url :redirect 10)
      (when (= status 200)
        (cl-csv:read-csv (babel:octets-to-string response))))))

(defmethod handle-command ((module stock-module)
                           (cmd (eql 'stock))
                           message args)
  "stock <symbol> - grab the latest price of a stock"
  (let ((symbols (or args '("GOOG"))))
    (cond
      ((notevery 'valid-stock-symbol-p symbols)
       (let ((bad-arg (find-if-not 'valid-stock-symbol-p args)))
         (reply-to message "Invalid stock symbol ~a" bad-arg)))
      ((> (length symbols) 5)
       (reply-to message "OMG! Too many ticker symbols!"))
      (t
       (let ((quote-list (retrieve-stock-quotes symbols)))
         (reply-to message "~:{~a ~a ~a: ~a (~a) $~a~%~}" quote-list))))))
