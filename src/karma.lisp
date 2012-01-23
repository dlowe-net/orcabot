(in-package #:orca)

(defmodule karma karma-module ("karma")
  (scores :accessor scores-of :initform (make-hash-table :test 'equalp)))

(defmethod initialize-module ((module karma-module) config)
  (clrhash (scores-of module))
  (with-open-file (inf (orca-path "data/karma.lisp") :direction :input)
    (loop for tuple = (read inf nil)
         while tuple
         do (setf (gethash (first tuple) (scores-of module))
                  (second tuple)))))

(defun save-karma-scores (module)
  (with-open-file (ouf (orca-path "data/karma.lisp")
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
  (if args
      (reply-to message "~a has ~a karma"
                (first args)
                (gethash (first args)
                         (scores-of module)
                         0))
      (reply-to message "Usage: ~karma <nick>")))
