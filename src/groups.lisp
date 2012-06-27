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

(defmodule groups groups-module ("group" "groupadd" "groupdel")
  (groups :accessor groups-of :initform nil))

(defun load-group-definitions (module)
  (with-open-file (inf (orcabot-path "data/groups.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (setf (groups-of module) (if inf (read inf nil) nil))))

(defun save-group-definitions (module)
  (with-open-file (ouf (orcabot-path "data/groups.lisp") :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write (groups-of module) :stream ouf)))

(defun group-definition-by-name (module name)
  (assoc name (groups-of module) :test #'string-equal))

(defun select-channel-nicks (connection channel-name nick-list)
  "Returns a list of the members of nick-list that are in the channel,
and a list of the member of nick-list that are not."
  (let ((channel (irc:find-channel connection channel-name)))
    (when channel
      (loop
         for channel-nick in (mapcar 'nickname (hash-values (users channel)))
         as normal-nick = (normalize-nick channel-nick)
         if (find normal-nick nick-list :test 'string-equal)
         collect channel-nick into online-nicks
         and
         collect normal-nick into normalized-nicks
         finally (return (values
                          (sort online-nicks #'string<)
                          (sort (set-difference nick-list normalized-nicks :test 'string=)
                                'string<)))))))

(defmethod initialize-module ((module groups-module)
                              config)
  (load-group-definitions module))

(defmethod handle-command ((module groups-module)
                           (cmd (eql 'group))
                           message args)
  "group (--list|<group name> [<message>]) - send a message to a group on the same channel"
  (cond
    ((null args)
     (reply-to message "You must specify a group~:[, but there aren't any!~;~]" (groups-of module)))
    ((string-equal (first args) "--list")
     (if (groups-of module)
         (reply-to message "Groups: ~{~a~^, ~}" (mapcar #'first (groups-of module)))
         (reply-to message "There are no groups.")))
    (t
     (let ((group-def (group-definition-by-name module (first args))))
       (multiple-value-bind (online-nicks offline-nicks)
           (select-channel-nicks (connection message)
                                 (first (arguments message))
                                 (rest group-def))
         (cond
           ((null group-def)
            (reply-to message "No such group '~a'" (first args)))
           ((null (rest args))
            (reply-to message "Group '~a'~@[ online: ~{~a~^ ~}~]~:[~;,~]~@[ offline: ~{~a~^ ~}~]"
                      (first group-def)
                      online-nicks
                      (and online-nicks offline-nicks)
                      offline-nicks))
           ((null online-nicks)
            (reply-to message "Nobody in group '~a' is here" (first args)))
           (t
            (reply-to message "~{~a~^,~}: ~{~a~^ ~}" online-nicks (rest args)))))))))

(defmethod handle-command ((module groups-module)
                           (cmd (eql 'groupadd))
                           message args)
  "groupadd <group name> <nicks> - add new group and/or nicks to the group"
  (cond
    ((null args)
     (reply-to message "You must specify a group and nicks to add"))
    ((null (rest args))
     (reply-to message "You must specify nicks to add"))
    ((null (assoc (first args)
                  (groups-of module)
                  :test #'string-equal))
     (push args (groups-of module))
     (save-group-definitions module)
     (reply-to message "Group '~a' added with nicks: ~{~a~^, ~}"
               (first args)
               (rest args)))
    (t
     (let* ((group-def (assoc (first args)
                              (groups-of module)
                              :test #'string-equal))
            (new-nicks (set-difference (rest args) (rest group-def) :test #'string=)))
       (cond
         (new-nicks
          (setf (rest group-def) (append (rest group-def) new-nicks))
          (save-group-definitions module)
          (reply-to message "Added ~{~a~^, ~} to group '~a'" new-nicks (first args)))
         (t
          (reply-to message "No new nicks added to group '~a'" (first args))))))))

(defmethod handle-command ((module groups-module)
                           (cmd (eql 'groupdel))
                           message args)
  "groupdel <group name> [<nicks>] - remove group or nicks from group"
  (cond
    ((null args)
     (reply-to message "You must specify a group and nicks to remove"))
    ((null (assoc (first args)
                  (groups-of module)
                  :test #'string-equal))
     (reply-to message "Group '~a' does not exist." (first args)))
    ((null (rest args))
     (let ((group-def (assoc (first args)
                             (groups-of module)
                             :test #'string-equal)))
       (setf (groups-of module) (delete group-def (groups-of module)))
       (save-group-definitions module)
       (reply-to message "group '~a': removed ~@[ (~{~a~^, ~})~]"
                 (first group-def)
                 (rest group-def))))
    (t
     (let* ((group-def (assoc (first args)
                              (groups-of module)
                              :test #'string-equal))
            (doomed-nicks (intersection (rest args)
                                        (rest group-def)
                                        :test #'string-equal))
            (invalid-nicks (set-difference (rest args)
                                           (rest group-def)
                                           :test #'string-equal)))
       ;; Remove group if all nicks are removed
       (cond
         ((subsetp (rest group-def) doomed-nicks :test #'string-equal)
          (setf (groups-of module) (delete group-def (groups-of module)))
          (reply-to message "group '~a': removed ~@[ (~{~a~^, ~})~]"
                    (first group-def)
                    (rest group-def)))
         (t
          (setf (rest group-def) (set-difference (rest group-def)
                                                 doomed-nicks
                                                 :test #'string-equal))
          (reply-to message "group '~a':~@[ removed ~{~a~^, ~}~]~:[~;, ~]~@[ ignored ~{~a~^, ~}~]"
                    (first group-def)
                    doomed-nicks
                    (and doomed-nicks invalid-nicks)
                    invalid-nicks)))

       (save-group-definitions module)))))