(in-package :orca)

;; Terms are stored in a hash table.  The key is the term and the
;; value is a list of response information.  Right now, the format of
;; the response is: (NICK LAST-MODIFIED-TIME DATA)

;; Terms are munged to provide support for relative pronouns.

(defvar *terms* (make-hash-table :test 'equalp))
(defvar *ignored-terms* nil)

(defun term-char-p (c)
  (or (alphanumericp c)
      (eql c #\space)
      (eql c #\')))

(defun munge-term (nick directp str)
  (let ((punct-pos (position-if-not 'term-char-p str :from-end t)))
    (when punct-pos
      (setf str (string-trim " " (subseq str (1+ punct-pos))))))
  (let ((text (cl-ppcre:split "\\s+" str)))
    (when directp
      (setf text (substitute "orca" "you" text :test #'string-equal))
      (setf text (substitute "orca's" "your" text :test #'string-equal)))
    (setf text (substitute (format nil "~a's" nick) "my" text :test #'string-equal))
    (setf text (substitute nick "i" text :test #'string-equal))
    (join-string " " text)))

(defun unmunge-term (nick str)
  (let ((text (cl-ppcre:split "\\s+" str)))
    (setf text (substitute "my" "orca's" text :test #'string-equal))
    (setf text (substitute "your" (format nil "~a's" nick) text :test #'string-equal))
    (join-string " " text)))

(defun save-terms ()
  (with-open-file (ouf "/home/dlowe/play/orca/data/terms.lisp"
                       :direction :output
                       :if-exists :rename-and-delete
                       :if-does-not-exist :create)
    (let ((terms (loop for x being the hash-keys of *terms* collect x)))
      (dolist (term (sort terms #'string<))
        (format ouf "(term ~s ~s)~%" term (gethash term *terms*)))
      (dolist (term *ignored-terms*)
        (format ouf "(ignore ~s)~%" term)))))

(defun load-terms ()
  (let ((*package* (find-package "ORCA")))
    (clrhash *terms*)
    (with-open-file (inf "/home/dlowe/play/orca/data/terms.lisp"
                         :direction :input)
      (loop
         for expr = (read inf nil)
         while expr
         do
           (case (first expr)
             (term
              (setf (gethash (second expr) *terms*)
                    (third expr)))
             (ignore
              (push (second expr) *ignored-terms*)))))))

(defun add-term (nick term def)
  (setf (gethash term *terms*) (list nick (now) def))
  (save-terms))

(defcommand describe (message directp &rest term-words)
  (let* ((term (munge-term (source message) directp (join-string " " term-words)))
         (def (gethash term *terms*)))
    (cond
      ((null def)
       (when directp
         (reply-to message (random-elt
                            '("No such term..."
                              "<LOUD ERROR MSG>..."
                              "No luck..."
                              "No match..."
                              "Drew a blank..."
                              "Never heard of it."
                              "You got me."
                              "Does not compute...")))))
      (t
       (reply-to message (random-elt
                          '("~a is, like, ~a"
                            "I heard that ~a is ~a"
                            "I think ~a is ~a"
                            "~a is ~a"
                            "hmm, ~a is ~a"
                            "From memory, ~a is ~a"))
                 term (third def))))))

(defcommand no (message directp &rest term-words)
  (let* ((is-pos (or (position "is" term-words :test #'string-equal)
                     (position "am" term-words :test #'string-equal)
                     (position "are" term-words :test #'string-equal)))
         (raw-term (and is-pos (join-string #\space (subseq term-words 0 is-pos))))
         (term (and raw-term (munge-term (source message) directp raw-term)))
         (def (and is-pos (join-string #\space
                                       (subseq term-words (1+ is-pos))))))
    (cond
      ((not (and is-pos (string/= "" term) (string/= "" def)))
       (when directp
         (reply-to message "What do you want me to remember?")))
      (t
       (add-term (source message) term def)
       (when directp
         (reply-to message "Ok, I've changed it."))))))

(defcommand remember (message directp &rest term-words)
  (let* ((is-pos (or (position "is" term-words :test #'string-equal)
                     (position "are" term-words :test #'string-equal)
                     (position "means" term-words :test #'string-equal)
                     (position "am" term-words :test #'string-equal)))
         (raw-term (and is-pos (join-string #\space (subseq term-words 0 is-pos))))
         (term (and raw-term (munge-term (source message) directp raw-term)))
         (def (and is-pos (join-string #\space
                                       (subseq term-words (1+ is-pos))))))
    (cond
      ((not (and is-pos (string/= "" term) (string/= "" def)))
       (when directp
         (reply-to message "What do you want me to remember?")))
      ((gethash term *terms*)
       (when directp
         (reply-to message "I already think ~a is ~a."
                (unmunge-term (source message) term)
                (third (gethash term *terms*)))))
      (t
       (add-term (source message) term def)
       (when directp
         (reply-to message "Ok, I'll remember that ~a is ~a."
                (unmunge-term (source message) term)
                def))))))

(defcommand forget (message directp &rest term-words)
  (let* ((term (join-string #\space term-words)))
    (when (string/= "" term)
      (remhash (munge-term (source message) directp term) *terms*)
      (save-terms)
      (when directp
        (reply-to message "I've forgotten all about ~a." term)))))

(defcommand ignore (message directp &rest term-words)
  (let* ((term (join-string #\space term-words)))
    (when (string/= "" term)
      (push term *ignored-terms*)
      (reply-to message "Ok, I'll ignore it when people say '~a?'" term))))