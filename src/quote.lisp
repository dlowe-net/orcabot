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

(defmodule quote quote-module ("quote" "quotedb")
  (quotes :accessor quotes-of :initform nil))

(defmethod initialize-module ((module quote-module) config)
  (load-quotes module))

(defun load-quotes (module)
  (setf (quotes-of module) nil)
  (with-open-file (inf (data-path "quotes.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (when inf
      (setf (quotes-of module) (loop for entry in (read inf)
                                    as idx from 1
                                    collect (cons idx entry))))))

(defun save-quotes (module)
  (write-to-file (data-path "quotes.lisp") (mapcar #'cdr (quotes-of module))))

(defmethod handle-command ((module quote-module)
                           (cmd (eql 'quote))
                           message args)
  "quote [<new quote>] - add a quote or get a random quote"
  (cond
    (args
     (let ((idx (if (quotes-of module)
                    (1+ (loop
                           for entry in (quotes-of module)
                           maximize (car entry)))
                    1)))
       (push (cons idx (format nil "~{~a~^ ~}" args))
             (quotes-of module))
       (save-quotes module)
       (reply-to message "Quote #~d added." idx)))
    ((null (quotes-of module))
     (reply-to message "There are no recorded quotes."))
    (t
     (reply-to message "~a" (cdr (random-elt (quotes-of module)))))))


(defmethod handle-command ((module quote-module)
                           (cmd (eql 'quotedb))
                           message args)
  "quotedb (reload|search|remove) - edit the quote database"
  (cond
    ((equal (first args) "reload")
     (load-quotes module)
     (reply-to message "Reloaded ~d quote~:p." (length (quotes-of module))))
    ((equal (first args) "search")
     (let ((patterns (mapcar (lambda (p)
                               (re:create-scanner p :case-insensitive-mode t))
                             (rest args)))
           (found 0))
       (loop
          for entry in (quotes-of module)
          when (every (lambda (p)
                        (re:scan p (cdr entry)))
                      patterns)
          do 
            (incf found)
            (when (<= found 5)
              (reply-to message "~d. ~a" (car entry) (cdr entry))))
       (cond
         ((zerop found)
          (reply-to message "No quotes found matching that pattern."))
         ((> found 5)
          (reply-to message "Found ~d quote~:p, but only displaying 5.  Try providing more search terms."
                    found)))))
    ((equal (first args) "remove")
     (let ((doomed (loop
                      for arg in (rest args)
                      for idx = (parse-integer arg :junk-allowed t)
                      if (null idx)
                      do (reply-to message "'~a' is not a valid quote id." arg)
                      else if (not (find idx (quotes-of module) :key #'car))
                      do (reply-to message "No quote found with id '~a'." arg)
                      else
                      collect idx)))
       (cond
         (doomed
          (setf (quotes-of module)
                (set-difference (quotes-of module) doomed :test (lambda (a b) (eql (car a) b))))
          (save-quotes module)
          (reply-to message "Quote~p ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~;, ~]~}~]~} removed." (length doomed) doomed))
         (t
          (reply-to message "No quotes removed.")))))
    (t
     (reply-to message "Usage: quotedb (search <text>|remove <id#>)"))))
