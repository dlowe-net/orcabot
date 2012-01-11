(in-package #:orca)

(defmodule basic basic-module ("help" "man"))

(defun command-documentation (cmd-module cmd)
  (let* ((cmd-symbol (intern (string-upcase cmd) (find-package "ORCA")))
         (method-object (find-method #'handle-command nil
                                     `(,(class-of cmd-module) (eql ,cmd-symbol) t t)
                                     nil)))
    (or (and method-object (documentation method-object t))
        "No documentation available")))

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