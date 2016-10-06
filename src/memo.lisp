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

(defmodule memo memo-module ("memo")
  (memos :accessor memos-of :initform (make-hash-table :test 'equalp)
         :documentation "Container for memo information.  Memos are
stored as NICK UNIVERSAL-TIME MESSAGE, with the destination user as
the key.
"))

(defmethod initialize-module ((module memo-module) config)
  (clrhash (memos-of module))
  (with-open-file (inf (data-path "memos.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (when inf
      (loop for tuple = (read inf nil)
         while tuple
         do (setf (gethash (first tuple) (memos-of module))
                  (rest tuple))))))

(defun save-memos (module)
  (with-open-file (ouf (data-path "memos.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (maphash (lambda (k v)
               (write (list* k v) :stream ouf)
               (terpri ouf))
             (memos-of module))))

(defun send-pending-memos (module nick)
  (let* ((user (normalize-nick nick))
         (memos (gethash user (memos-of module))))
    (when memos
      (log:log-message :info "Sending ~a pending memos to ~a" (length memos) user)
      (dolist (memo memos)
        (irc:privmsg (conn-of module)
                     nick
                     (format nil "~a said ~a ago: ~a"
                             (first memo)
                             (describe-duration (- (get-universal-time)
                                                   (second memo)))
                             (third memo))))
      (remhash user (memos-of module))
      (save-memos module))))

(defun add-new-memo (module from-nick to-nick message)
  (push (list from-nick
              (get-universal-time)
              message)
        (gethash (normalize-nick to-nick) (memos-of module)))
  (save-memos module))

(defmethod examine-message ((module memo-module)
                           (message irc:ctcp-action-message))
  (send-pending-memos module (source message)))

(defmethod examine-message ((module memo-module)
                           (message irc:irc-nick-message))
  (send-pending-memos module (source message)))

(defmethod examine-message ((module memo-module)
                           (message irc:irc-part-message))
  (send-pending-memos module (source message)))

(defmethod examine-message ((module memo-module)
                           (message irc:irc-join-message))
  (send-pending-memos module (source message)))

(defmethod handle-command ((module memo-module)
                           (cmd (eql 'memo))
                           message args)
  "memo <nick> <message> - leave a memo for nick "
  (cond
    ((< (length args) 2)
     (reply-to message "Usage: memo <nick> <message>"))
    ((not (valid-nick-p (first args)))
     (reply-to message "'~a' is not a valid nick." (first args)))
    (t
     (add-new-memo module (source message)
                   (first args)
                   (format nil "~{~a~^ ~}" (rest args)))
     (reply-to message "Memo left for ~a." (first args)))))
