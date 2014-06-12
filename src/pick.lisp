(in-package #:orcabot)

(defmodule pick pick-module ("pick"))

(defmethod handle-command ((module pick-module)
                           (cmd (eql 'pick))
                           message args)
  ".pick <choice>,[<choice>...] - selects randomly between choices"
  (let* ((raw-msg (second (arguments message)))
         (args (cl-ppcre:regex-replace "^\\S+\\s*" raw-msg ""))
         (choices (cl-ppcre:split "\\s*,\\s*" args)))
    (cond
      ((endp choices)
       (reply-to message "~a: Pick out of what choices?" (source message)))
      (t
       (reply-to message "I pick ~a!" (alexandria:random-elt choices))))))
