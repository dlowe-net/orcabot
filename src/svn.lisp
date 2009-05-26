(in-package :orca)

(defun string-limit (str max-len)
  (string-trim '(#\space)
               (if (< (length str) max-len)
                   str
                   (let ((pivot (position-if-not #'alphanumericp
                                                 str
                                                 :from-end t
                                                 :end max-len)))
                     (concatenate 'string
                                  (subseq str 0 (or pivot max-len))
                                  "...")))))

(defun retrieve-svn-log (rev)
  (let ((log (with-output-to-string (str)
                (sb-ext:run-program "/usr/bin/svn"
                                    `("log" "-r" ,rev "svn+ssh://svn/svn/ita")
                                    :input nil :output str))))
    (ppcre:register-groups-bind (user message)
        ((ppcre:create-scanner "-+\\nr\\d+ \\| (.*) \\| [^(]+\\([^)]+\\) \\| \\d+ lines\\n\\n(.*?)^-{70,}" :single-line-mode t :multi-line-mode t)
         log)
      (format nil "~a - ~a" user
              (string-limit (substitute #\space #\newline message) 160)))))

(defcommand svn (message directp rev)
  (multiple-value-bind (match regs)
      (ppcre:scan-to-strings "[#r]?(\\d+)" rev)
    (cond
      ((null match)
       (reply-to message "I'd rather have a revision number."))
      (t
       (let ((subject (retrieve-svn-log (aref regs 0))))
         (cond
           (subject
            (reply-to message
                      "svn r~a is ~a (https:///trac/changeset/~a)"
                      (aref regs 0) subject (aref regs 0)))
           (directp
            (reply-to message "SVN r~a doesn't seem to exist" (aref regs 0)))))))))