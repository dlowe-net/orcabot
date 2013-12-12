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

(defmodule quote quote-module ("quote")
  (quotes :accessor quotes-of :initform nil))

(defmethod initialize-module ((module quote-module) config)
  (setf (quotes-of module) nil)
  (with-open-file (inf (data-path "quotes.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (when inf
      (setf (quotes-of module) (read inf)))))

(defun save-quotes (module)
  (with-open-file (ouf (data-path "quotes.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write (quotes-of module) :stream ouf)
    (terpri ouf)))

(defmethod handle-command ((module quote-module)
                           (cmd (eql 'quote))
                           message args)
  "quote [<new quote>] - add a quote or get a random quote"
  (cond
    (args
     (push (format nil "~{~a~^ ~}" args)
           (quotes-of module))
     (save-quotes module)
     (reply-to message "Quote added."))
    ((null (quotes-of module))
     (reply-to message "There are no recorded quotes."))
    (t
     (reply-to message "~a" (random-elt (quotes-of module))))))
