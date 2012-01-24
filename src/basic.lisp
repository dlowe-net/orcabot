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

(in-package #:orca)

(defmodule basic basic-module ("about" "help" "man"))

(defun command-documentation (cmd-module cmd)
  (let* ((cmd-symbol (intern (string-upcase cmd) (find-package "ORCA")))
         (method-object (find-method #'handle-command nil
                                     `(,(class-of cmd-module) (eql ,cmd-symbol) t t)
                                     nil)))
    (or (and method-object (documentation method-object t))
        "No documentation available")))

(defmethod handle-command ((module basic-module) (cmd (eql 'about)) message args)
  "about - display information about orca"
  (reply-to message "Orcabot version 2.0 / Daniel Lowe <dlowe@google.com>"))

(defmethod handle-command ((module basic-module) (cmd (eql 'help)) message args)
  "help [<command>] - display orca help"
  (let* ((cmd-str (first args))
         (cmd-module (find-if (lambda (module)
                                (member cmd-str (commands-of module)
                                        :test #'string-equal))
                              *orca-modules*)))
    (cond
      ((and cmd-module (not (access-denied cmd-module message)))
       (reply-to message "~a" (command-documentation cmd-module cmd-str)))
      (t
       (reply-to message "Help is available for the following commands: ~{~a~^ ~}"
                 (sort
                  (loop
                     for mod in *orca-modules*
                     unless (access-denied mod message)
                     appending (loop for cmd in (commands-of mod)
                                  collect cmd))
                  #'string<))))))

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