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

(defmodule basic basic-module ("man"))

(defmethod handle-message ((module basic-module)
                           (message irc:irc-invite-message))
  (irc:join (connection message) (second (arguments message))))

(defmethod handle-command ((module basic-module) (cmd (eql 'man)) message args)
  "man <term> - look up term in unix manual"
  (let ((output (with-output-to-string (str)
                  (sb-ext:run-program "/usr/bin/whatis"
                                      (list (first args))
                                      :input nil :output str))))
    (if (search "nothing appropriate" output)
        (reply-to message "Nothing found for ~a" (first args))
        (ppcre:register-groups-bind (section desc)
            ((ppcre:create-scanner "^\\S+\\s+\\((\\d+)\\)\\s+- (.*)"
                                   :multi-line-mode t) output)
          (reply-to message "~a - ~a [http://linuxmanpages.com/man~a/~a.~a.php]"
                    (first args) desc section (first args) section)))))
