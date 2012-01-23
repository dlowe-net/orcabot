(in-package :orca)

(defmodule itasvn itasvn-module ("svn"))

(defun retrieve-svn-log (rev)
  (let ((log (with-output-to-string (str)
                (sb-ext:run-program "/usr/bin/svn"
                                    `("log" "-r" ,rev "https:///svn/ita/")
                                    :input nil :output str))))
    (ppcre:register-groups-bind (user message)
        ((ppcre:create-scanner "-+\\nr\\d+ \\| (.*) \\| [^(]+\\([^)]+\\) \\| \\d+ lines?\\n\\n(.*?)^-{70,}" :single-line-mode t :multi-line-mode t)
         log)
      (format nil "~a - ~a" user
              (string-limit (substitute #\space #\newline message) 160)))))

(defun retrieve-svn-last-commit-log (path)
  (let ((log (with-output-to-string (str)
                (sb-ext:run-program "/usr/bin/svn"
                                    `("log" "--limit" "1"
                                            ,(format nil "https:///svn/ita/~a" path))
                                    :input nil :output str))))
    (ppcre:register-groups-bind (rev user message)
        ((ppcre:create-scanner "-+\\nr(\\d+) \\| (.*) \\| [^(]+\\([^)]+\\) \\| \\d+ lines?\\n\\n(.*?)^-{70,}" :single-line-mode t :multi-line-mode t)
         log)
      (format nil "r~a - ~a - ~a https:///trac/changeset/~a" rev user
              (string-limit (substitute #\space #\newline message) 160)
              rev))))

(defun lookup-all-svn (message rev-numbers)
  (let ((match-found nil))
    (dolist (rev (remove-duplicates rev-numbers :test #'string=))
      (multiple-value-bind (match regs)
          (ppcre:scan-to-strings "[#r]?(\\d+)" rev)
        (cond
          (match
              (let ((subject (retrieve-svn-log (aref regs 0))))
                (if subject
                    (reply-to message
                              "svn r~a is ~a https:///trac/changeset/~a"
                              (aref regs 0) subject (aref regs 0))
                    (reply-to message "SVN r~a doesn't seem to exist" (aref regs 0)))))
          (t
           (let ((info (retrieve-svn-last-commit-log rev)))
             (if info
                (reply-to message
                          "~a last modified ~a"
                          rev info)
                (reply-to message "I'd rather have a revision number or path.")))))))
    match-found))

(defmethod handle-message ((module itasvn-module) (type (eql 'irc:irc-privmsg-message)) message)
  (dolist (revnum (all-matches-register
                   (ppcre:create-scanner "\\b(?:svn [:#]*|r)(\\d{6,})(?:$|[^\\d-])"
                                         :case-insensitive-mode t)
                   (second (arguments message)) 0
                   :sharedp t))
    (let ((subject (retrieve-svn-log revnum)))
      (when subject
          (reply-to message
                    "svn r~a is ~a [https:///trac/changeset/~a]"
                    revnum subject revnum))))
  nil)

(defmethod handle-command ((module itasvn-module) (cmd (eql 'svn))
                           message
                           args)
  "svn <revision number/path> - show a link to an ITA subversion revision"
  (let* ((revnums (if (string-equal (first args) "topic")
                      (all-matches-register "r(\\d+)"
                                            (topic (find-channel (connection message)
                                                                 (first (arguments message)))) 0
                                                                 :sharedp t)
                      args)))

    (cond
      ((null revnums)
       (reply-to message "I'd rather have a revision number or path"))
      (t
       (lookup-all-svn message revnums)))))
