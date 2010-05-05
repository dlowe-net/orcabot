(in-package :orca)

(defvar *parrots* (make-hash-table :test 'equalp))

(defun markov-learn (corpus text)
  (let ((words (cl-ppcre:split "\\s+" text)))
    (loop for (first second) on words by #'cdr
       while (and first second)
       do (push second (gethash (list first) corpus)))))

(defun markov-generate (corpus count)
  (let ((result (random-elt (hash-keys corpus))))
    (loop
       repeat count
       for branches = (gethash (list (first result)) corpus)
       while branches
       do (push (random-elt branches) result))
    (nreverse result)))

(defun parrot-learn (nick text)
  (setf nick (string-trim "_-" nick))
  (let ((badchar-pos (or (position #\_ nick :from-end t)
                         (position #\- nick :from-end t)
                         (position #\[ nick :from-end t))))
    (when badchar-pos
      (setf nick (subseq nick 0 (1- badchar-pos)))))
  (unless (string= "" nick)
    (let ((parrot (gethash nick *parrots*)))
      (unless parrot
        (setf parrot (make-hash-table :test 'equal))
        (setf (gethash nick *parrots*) parrot))
      (markov-learn parrot text))))

(defun parrot-speak (nick)
  (let ((parrot (gethash nick *parrots*)))
    (if parrot
        (format nil "<~a> ~a" nick (join-string " " (markov-generate parrot 100)))
        (format nil "Never heard of ~a" nick))))

(defun parrots-learn-from-line (line)
  (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings
       "<span class=\"irc-black\">&lt;([^[&_-]+)[^&]*&gt; (.*)</span><br />"
       line :sharedp t)
    (when match
      (parrot-learn (aref regs 0) (aref regs 1)))))

(defun parrots-learn-from-file (path)
  (with-open-file (inf path :direction :input)
    (loop
       for line = (read-line inf nil)
       while line
       do (parrots-learn-from-line line))))

(defun parrots-learn-from-dir (dir-path)
  (clrhash *parrots*)
  (dolist (file-path (directory dir-path))
    (format t "Learning from ~a~%" file-path)
    (parrots-learn-from-file file-path)))

(define-fun-command parrot (message directp nick)
  (when directp
    (let ((msg (parrot-speak nick)))
      (if (char= #\# (char (first (arguments message)) 0))
          (irc:privmsg (connection message) (first (arguments message)) msg)
          (irc:privmsg (connection message) (source message) msg)))))

(defun save-parrots ()
  (with-open-file (ouf (orca-path "data/parrots.lisp")
                       :direction :output
                       :if-exists :rename-and-delete
                       :if-does-not-exist :create)
    (dolist (parrot-nick (sort (hash-keys *parrots*) #'string<))
      (let ((parrot (gethash parrot-nick *parrots*)))
        (format ouf "(parrot ~s ~s)~%"
                parrot-nick
                (loop
                   for key in (hash-keys parrot)
                   collect (list key (gethash key parrot))))))))

(defun load-parrots ()
  (clrhash *parrots*)
  (let ((*package* (find-package "ORCA")))
    (with-open-file (inf (orca-path "data/parrots.lisp"))
      (loop
         for parrot-spec = (read inf nil)
         while parrot-spec
         when (eql (first parrot-spec) 'parrot)
         do
           (let ((parrot (make-hash-table :test 'equal)))
             (setf (gethash (second parrot-spec) *parrots*) parrot)
             (dolist (tuple (third parrot-spec))
               (setf (gethash (first tuple) parrot) (second tuple))))))))

(defun parrot-analyze (nick)
  (let ((parrot (gethash nick *parrots*)))
    (loop for key in (sort (hash-keys parrot)
                           (lambda (a b)
                             (> (length (gethash a parrot))
                                (length (gethash b parrot)))))
         repeat 10
         do (format t "~s ~s~%" key (gethash key parrot)))))

(defun textify-irc-logs (dir-path)
  (dolist (inf-path (directory dir-path))
    (format t "Textifying ~a~%" inf-path)
    (with-open-file (inf inf-path)
      (with-open-file (ouf (make-pathname
                            :type "txt"
                            :defaults inf-path)
                           :direction :output
                           :if-exists :rename-and-delete
                           :if-does-not-exist :create)
        (loop
           for line = (read-line inf nil)
           while line
           do
             (cl-ppcre:register-groups-bind (hour minute nick message)
                 ("<span class=\"irc-date\">\\[(\\d+):(\\d+)\\]</span> <span class=\"irc-black\">&lt;([^[&_-]+)[^&]*&gt; (.*)</span><br />" line :sharedp t)
               (when (and hour minute)
                 (format ouf "[~2d:~2d] <~a> ~a~%"
                         (parse-integer hour)
                         (parse-integer minute)
                         nick
                         message)))
             (cl-ppcre:register-groups-bind (hour minute message)
                 ("<span class=\"irc-date\">\\[(\\d+):(\\d+)\\]</span> <span class=\"irc-brick\">(\\* .*)</span><br />" line :sharedp t)
               (when (and hour minute)
                 (format ouf "[~2d:~2d] ~a~%"
                         (parse-integer hour)
                         (parse-integer minute)
                         message))))))))