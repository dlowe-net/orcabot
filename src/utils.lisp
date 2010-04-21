(in-package :orca)

(defvar *command-funcs* (make-hash-table :test 'equalp))

(defmacro defcommand (name args &body body)
  (let ((tmp-args (gensym "TMP-ARGS"))
        (msg-sym (first args))
        (direct-sym (second args))
        (args-sym (cons '&optional (cddr args))))
    `(progn
       (setf (gethash ,(string name) *command-funcs*)
             (lambda (,msg-sym ,direct-sym ,tmp-args)
               (declare (ignorable ,msg-sym ,direct-sym))
               (destructuring-bind ,args-sym ,tmp-args ,@body))))))

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

(defun reply-to (message fmt &rest args)
  (let ((response (format nil "~?" fmt args)))
    (if (char= #\# (char (first (arguments message)) 0))
        (irc:privmsg *connection* (first (arguments message)) response)
        (irc:privmsg *connection* (source message) response))))

(defun authentication-credentials (host)
  (flet ((read-word (stream)
           (when (peek-char t stream nil)
             (with-output-to-string (s)
               (loop
                  for c = (read-char stream nil)
                  while (and c
                             (char/= c #\newline)
                             (char/= c #\space)) do
                    (princ c s))))))
    (let ((found-machine nil)
          (result nil))
      (with-open-file (inf (merge-pathnames (user-homedir-pathname)
                                            ".netrc")
                           :direction :input)
        (loop
           for key = (read-word inf)
           as val = (read-word inf)
           while val do
             (cond
               ((string-equal key "machine")
                (setf found-machine (string-equal val host)))
               (found-machine
                (push val result)
                (push (intern (string-upcase key) (find-package "KEYWORD")) result))))
        result))))


