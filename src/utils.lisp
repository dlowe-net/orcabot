(in-package :orca)

(defvar *nickname*)

(defparameter *orca-root-pathname*
    (asdf:component-pathname (asdf:find-system "orca")))

(define-condition orca-exiting () ())
(define-condition no-such-module (error) ())

(defvar *command-funcs* (make-hash-table :test 'equalp))

(defun orca-path (fmt &rest args)
  "Returns the local pathname merged with the root package path."
  (let ((path (if args
                  (format nil "~?" fmt args)
                  fmt)))
    (merge-pathnames path *orca-root-pathname*)))

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
  (let* ((raw-response (format nil "~?" fmt args))
         (response (string-limit raw-response 500)))
    (cond
      ((char= #\# (char (first (arguments message)) 0))
       (irc:privmsg (connection message) (first (arguments message)) response))
      (t
       (irc:privmsg (connection message) (source message) response)))))

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
                (push (intern (string-upcase key) :keyword) result))))
        result))))

(defun shorten-nick (full-nick)
  (ppcre:scan-to-strings "[A-Za-z]+" full-nick))

(defun message-target-is-channel-p (message)
  (find (char (first (arguments message)) 0) "#+"))

(defun all-matches-register (regex target-string register
                             &key (start 0)
                             (end (length target-string))
                             (sharedp nil))
  (let ((substr-fn (if sharedp #'ppcre::nsubseq #'subseq))
        (result-list nil))
      (ppcre:do-scans (start end reg-start reg-end
                             regex target-string
                             result-list
                             :start start :end end)
        (push (funcall substr-fn
                       target-string
                       (aref reg-start register)
                       (aref reg-end register))
              result-list))))

(defun switch-person (str)
  (cl-ppcre:regex-replace-all
   (cl-ppcre:create-scanner "\\b(mine|me|my|I am|I'm|I|you are|you're|yours|your|you)\\b" :case-insensitive-mode t)
   str
   (lambda (target start end match-start match-end reg-starts reg-ends)
     (declare (ignore start end reg-starts reg-ends))
     (let ((match (make-array (list (- match-end match-start)) :element-type 'character :displaced-to target :displaced-index-offset match-start)))
       (cond
         ((string-equal "I" match)
          "you")
         ((string-equal "me" match)
          "you")
         ((string-equal "my" match)
          "your")
         ((string-equal "I am" match)
          "you are")
         ((string-equal "I'm" match)
          "you're")
         ((string-equal "mine" match)
          "yours")
         ((string-equal "you" match)
          "I")
         ((string-equal "your" match)
          "my")
         ((string-equal "yours" match)
          "mine")
         ((string-equal "you're" match)
          "I'm")
         ((string-equal "you are" match)
          "I am"))))))

(defun make-random-list (length max)
  "Generate a non-repeating list of random numbers of length LENGTH.  The maxim\
um value of the random numbers is MAX - 1."
  (declare (fixnum length max))
  (unless (plusp length)
    (error "LENGTH may not be negative."))
  (unless (plusp max)
    (error "Can't generate negative numbers."))
  (unless (<= length max)
    (error "Can't generate a non-repeating list when LENGTH > MAX"))
  (let ((unused (make-array max :element-type 'fixnum)))
    ;; create an array with each element set to its index
    (dotimes (idx max)
      (setf (aref unused idx) idx))
    ;; select a random index to pull from the array, then set the number
    ;; at the index to the last selectable element in the array.  Continue
    ;; until we have the requisite list length
    (loop for result-size from 0 upto (1- length)
          as num = (random (- max result-size))
          collect (aref unused num)
          do (setf (aref unused num) (aref unused (- max result-size 1))))))


