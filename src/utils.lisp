(in-package :orca)

(defmacro defcommand (name args &body body)
  (let ((tmp-args (gensym "TMP-ARGS"))
        (msg-sym (first args))
        (direct-sym (second args))
        (args-sym (cons '&optional (cddr args))))
    `(progn
       (setf (gethash ,(string name) *command-funcs*)
             (lambda (,msg-sym ,direct-sym ,tmp-args)
               (declare (ignorable ,msg-sym ,direct-sym))
               (flet ((reply (fmt &rest args)
                        (let ((response (format nil "~?" fmt args)))
                          (if (char= #\# (char (first (arguments message)) 0))
                              (irc:privmsg *connection* (first (arguments message))
                                           (format nil "~a: ~a" (source message) response))
                              (irc:privmsg *connection* (source message) response)))))
                 (destructuring-bind ,args-sym ,tmp-args ,@body)))))))

(defun random-elt (sequence)
  (elt sequence (random (length sequence))))

(defun hash-keys (hash)
  (loop for key being the hash-keys of hash
       collect key))

(defun hash-values (hash)
  (loop for value being the hash-values of hash
       collect value))

(defun join-string (delimiter list)
  (format nil (format nil "~~{~~a~~^~a~~}" delimiter) list))

(defun starts-with (string prefix)
  (when (>= (length string) (length prefix))
    (string= prefix string :end2 (length prefix))))

(defun ends-with (string suffix)
  (when (>= (length string) (length suffix))
    (string= suffix string :start2 (- (length string) (length suffix)))))

(defun strip-prefixes (string &rest prefixes)
  (dolist (prefix prefixes)
    (when (starts-with string prefix)
      (return-from strip-prefixes (subseq string (length prefix)))))
  string)