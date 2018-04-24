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

(defmodule stock stock-module ("stock")
  (api-key :accessor api-key-of :initform nil))

(defmethod initialize-module ((module stock-module) config)
  (let ((module-conf (rest (assoc 'stock config))))
    (setf (api-key-of module) (getf module-conf :api-key)))

  (when (null (api-key-of module))
    (log:log-message :warning "WARNING: Missing API key for STOCK module~%")))

(defun valid-stock-symbol-p (str)
  (every (lambda (c)
           (or (alpha-char-p c)
               (eql c #\^))) str))

(defun stock-quote-retriever (api-key)
  (lambda (symbol)
    (multiple-value-bind (response status)
        (drakma:http-request "https://www.alphavantage.co/query"
                             :parameters `(("function" . "TIME_SERIES_DAILY")
                                           ("symbol" . ,symbol)
                                           ("apikey" . ,api-key)
                                           ("datatype" . "csv")))
      (when (= status 200)
        (destructuring-bind (date open high low closing volume)
            ;; first record is headers, second record is most recent day
            (second (cl-csv:read-csv (babel:octets-to-string response)))
          (declare (ignore high low volume))
          (let ((closing (parse-integer closing :junk-allowed t))
                (open (parse-integer open :junk-allowed t)))
            (list
             date
             (string-upcase symbol)
             closing
             (float (/ (* 100 (- closing open)) open)))))))))

(defun retrieve-stock-quotes (api-key symbols)
  (mapcar (stock-quote-retriever api-key) symbols))

(defmethod handle-command ((module stock-module)
                           (cmd (eql 'stock))
                           message args)
  "stock <symbol> - grab the latest price of a stock"
  (let ((symbols (or args '("GOOG"))))
    (cond
      ((null (api-key-of module))
       (reply-to message "API key not set."))
      ((notevery 'valid-stock-symbol-p symbols)
       (let ((bad-arg (find-if-not 'valid-stock-symbol-p args)))
         (reply-to message "Invalid stock symbol ~a" bad-arg)))
      ((> (length symbols) 5)
       (reply-to message "OMG! Too many ticker symbols!"))
      (t
       (let ((quote-list (retrieve-stock-quotes (api-key-of module) symbols)))
         (reply-to message "~:{~a ~a: ~a (~,2@f%)~%~}" quote-list))))))
