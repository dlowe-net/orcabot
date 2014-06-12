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

(defparameter +learn-regexes+ (mapcar (lambda (p)
                                        (cl-ppcre:create-scanner p :case-insensitive-mode t))
                                      '("(.*?)\\s+<(\\S+)>\\s*(.*)"
                                        "(.*?)\\s+(is|are|am|isn't|aren't)\\s+(.*)")))
(defparameter +address-regexes+ (mapcar (lambda (p)
                                          (list* (cl-ppcre:create-scanner (first p) :case-insensitive-mode t)
                                                 (rest p)))
                                      '(("^([^:, ]+)[:,]\\s*(.*)" 0 1)
                                        ("(.*),\\s*(.*)$" 1 0))))

(defun make-response-db ()
  (make-hash-table :test 'equal))

(defun load-response-db (db path)
  (clrhash db)
  (dolist (x (with-open-file (inf path :direction :input :if-does-not-exist nil)
               (when inf
                 (let ((*package* (find-package :orcabot)))
                   (read inf)))))
    (setf (gethash (first x) db) (rest x))))

(defun save-response-db (db path)
  (write-to-file path
                 (mapcar (lambda (trigger)
                           (cons trigger (gethash trigger db)))
                         (sort (alexandria:hash-table-keys db)
                               #'string<))))

(defun get-addressed-text (nick channel text)
  (dolist (r +address-regexes+)
    (destructuring-bind (regex name-reg text-reg) r
      (multiple-value-bind (match-begin match-end reg-begin reg-end)
          (cl-ppcre:scan regex text)
        (declare (ignore match-end))
        (when (and match-begin
                   (string-equal nick
                                 text
                                 :start2 (aref reg-begin name-reg)
                                 :end2 (aref reg-end name-reg)))
          (return-from get-addressed-text (subseq text
                                                  (aref reg-begin text-reg)
                                                  (aref reg-end text-reg)))))))
  (when (string= nick channel)
    text))

(defun add-alias-response (db trigger alias)
  (loop
     for alias-check = alias then (cadar response)
     as response = (gethash alias-check db)
     while (or (null response)
               (eql (caar response) :alias))
     do
       (when (null response)
         (return-from add-alias-response nil))
       (when (equalp (cadar response) trigger)
         (return-from add-alias-response nil)))
  
  (setf (gethash (string-downcase trigger) db)
        (list (list :alias alias))))

(defun add-response (db trigger action response)
  (let ((existing-response (gethash (string-downcase trigger) db)))
    (when (eql (caar existing-response) :alias)
      (setf (gethash (string-downcase trigger) db) nil)))
  (log:log-message :info "(respond) learned ~s => ~a ~s" trigger action response)
  (pushnew (list action response)
           (gethash (string-downcase trigger) db)
           :test #'equal))

(defun extract-learnable (db source text)
  (dolist (regex +learn-regexes+)
    (multiple-value-bind (match regs)
        (cl-ppcre:scan-to-strings regex text)
      (when match
        (let ((fact (aref regs 0))
              (verb (aref regs 1))
              (tidbit (aref regs 2)))
          (cond
            ((and (equalp fact "I")
                  (equalp verb "am"))
             (setf fact source
                   verb "is"))
            ((and (equalp fact "you")
                  (equalp verb "are"))
             (setf fact "orca"
                   verb "is"))
            ((and (equalp fact "you")
                  (equalp verb "aren't"))
             (setf fact "orca"
                   verb "isn't")))
          (return-from extract-learnable
            (cond
              ((equal verb "action")
               (add-response db fact :action tidbit))
              ((equal verb "alias")
               (add-alias-response db fact tidbit))
              ((equal verb "reply")
               (add-response db fact :reply tidbit))
              ((equal verb "'s")
               (add-response db fact :reply (format nil "~a's ~a" fact tidbit)))
              (t
               (add-response db fact :reply (format nil "~a ~a ~a" fact verb tidbit)))))))))

  ;; no learnable extracted
  nil)

(defun select-response (db trigger)
  (loop
     for responses = (gethash (string-downcase trigger) db)
     while responses
     do
       (let ((response (random-elt responses)))
         (if (eql (first response) :alias)
             (setf trigger (second response))
             (return-from select-response response)))))

(defmodule respond respond-module ("learn" "lookup")
  (responses :accessor responses-of)    ; hashtable of responses to exact matches
  (regexes :accessor regexes-of))       ; ordered list of regular expressions to match

(defmethod initialize-module ((module respond-module) config)
  ;; regexes loaded by configuration
  (setf (regexes-of module)
        (map 'list
             (lambda (x)
               (cons (ppcre:create-scanner (first x)
                                           :case-insensitive-mode t)
                     (rest x)))
             (rest (assoc 'respond config))))

  ;; exact responses loaded from separate file
  (setf (responses-of module) (make-response-db))
  (load-response-db (responses-of module) (data-path "responses.lisp")))

(defmethod handle-message ((module respond-module)
                           (message irc:irc-privmsg-message))
  (destructuring-bind (channel text)
      (arguments message)
    
    ;; Always do regex matches
    (loop
       for response in (regexes-of module)
       as match = (ppcre:scan (first response) text)
       when match
       do
         (let ((selected-response (random-elt (rest response))))
           (log:log-message :info "(respond) ~a saying ~s triggered ~s" (source message) text selected-response)
           (reply-to message "~a" selected-response)))

    ;; When addressed, learn a new response or emit a response
    (let ((addressed-text (get-addressed-text (nickname (user (connection message))) channel text)))
      (when addressed-text
        (cond
          ((extract-learnable (responses-of module)
                              (source message)
                              addressed-text)
           (save-response-db (responses-of module) (data-path "responses.lisp"))
           (reply-to message "~a: Okay." (source message)))
          (t
           (let ((response (select-response (responses-of module)
                                            addressed-text)))
             (when response
               (log:log-message :info "(respond) ~a saying ~s triggered ~s" (source message) addressed-text response)
               (case (first response)
                 (:reply
                  (reply-to message "~a" (second response)))
                 (:action
                  (action-to message "~a" (second response)))))))))))
  nil)

(defmethod handle-command ((module respond-module)
                           (command (eql 'lookup))
                           message args)
  "lookup <term> - Gives the responses learned for <term>"
  (let* ((text (string-downcase (join-to-string " " args)))
         (response (or (select-response (responses-of module) text)
                       (select-response (responses-of module) "don't know")
                       '(:reply "No idea!  Sorry!"))))
    (reply-to message "~a: ~a" (source message) (second response))))

(defmethod handle-command ((module respond-module)
                           (command (eql 'learn))
                           message args)
  "learn <term> ( am | is | isn't | are | aren't | < <verb> > | < reply > ) <tidbit> - learn a new response"
  (let ((text (join-to-string " " args)))
    (cond
      ((extract-learnable (responses-of module)
                           (source message)
                           text)
       (save-response-db (responses-of module) (data-path "responses.lisp"))
       (reply-to message "~a: Okay" (source message)))
      (t
        (reply-to message "~a: I totally did not understand that." (source message))))))
