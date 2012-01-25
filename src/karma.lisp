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

(defmodule karma karma-module ("karma")
  (scores :accessor scores-of :initform (make-hash-table :test 'equalp)))

(defmethod initialize-module ((module karma-module) config)
  (clrhash (scores-of module))
  (with-open-file (inf (orcabot-path "data/karma.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (when inf
      (loop for tuple = (read inf nil)
         while tuple
         do (setf (gethash (first tuple) (scores-of module))
                  (second tuple))))))

(defun save-karma-scores (module)
  (with-open-file (ouf (orcabot-path "data/karma.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (maphash (lambda (k v)
               (write (list k v) :stream ouf)
               (terpri ouf))
             (scores-of module))))

(defmethod handle-message ((module karma-module) (type (eql 'irc:irc-privmsg-message))
                           message)
  ;; Add karma to nick.  Take karma away if they're trying to give
  ;; themselves karma
  (ppcre:register-groups-bind (nick)
      ("^(\\S+)\\+\\+$" (second (arguments message)))
    (if (string-equal nick (source message))
        (decf (gethash nick (scores-of module) 0))
        (incf (gethash nick (scores-of module) 0)))
    (save-karma-scores module))

  ;; Take karma away from nick
  (ppcre:register-groups-bind (nick)
      ("^(\\S+)\\-\\-$" (second (arguments message)))
    (decf (gethash nick (scores-of module) 0))
    (save-karma-scores module))
  nil)

(defmethod handle-command ((module karma-module)
                           (cmd (eql 'karma))
                           message args)
  "karma <nick> - check the karma in the soul"
  (if args
      (reply-to message "~a has ~a karma"
                (first args)
                (gethash (first args)
                         (scores-of module)
                         0))
      (reply-to message "Usage: ~karma <nick>")))
