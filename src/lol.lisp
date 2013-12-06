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

(defvar *lol-db* (make-hash-table :test 'equalp))

(defun load-lol-db (path)
  (clrhash *lol-db*)
  (with-open-file (inf path :direction :input)
    (loop
       for tuple = (read inf nil)
       while tuple
       do (setf (gethash (first tuple) *lol-db*) (second tuple)))))

(defun lol-translate (text)
  (cl-ppcre:regex-replace-all
   "(\\s)\\1+"
   (cl-ppcre:regex-replace-all
    "\\w+" text
    (lambda (target start end match-start match-end reg-starts reg-ends)
      (declare (ignore start end reg-starts reg-ends))
      (let ((match (make-array (list (- match-end match-start)) :element-type 'character :displaced-to target :displaced-index-offset match-start)))
        (gethash match *lol-db* match))))
   "\\1"))

(define-fun-command lolsay (message directp &rest text)
  (reply-to message (lol-translate (join-to-string #\space text))))

(define-fun-command lolize (message directp nick)
  (let ((last-said (fourth
                    (find (first (arguments message))
                          (gethash nick *last-said*)
                          :test #'string=
                          :key #'third))))
    (if last-said
        (reply-to message "<~a> ~a" nick (lol-translate last-said))
        (reply-to message "~a: I no haz teh werds of ~a" (source message) nick))))