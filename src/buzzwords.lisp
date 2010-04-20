(in-package #:orca)

(defvar *buzzwords* (make-hash-table))
(defvar *grammar* nil)

(defun buzzword-load ()
  (let ((*package* (find-package "ORCA")))
    (setf *grammar*
          (with-open-file (inf "/home/dlowe/play/orca/data/grammar.lisp")
            (loop
               for form = (read inf nil)
               while form
               collect form)))
    (clrhash *buzzwords*)
    (with-open-file (inf "/home/dlowe/play/orca/data/buzzwords.lisp")
      (loop
         for form = (read inf nil)
         while form do
           (push (second form) (gethash (first form) *buzzwords*))))))

(defun buzzword (grammar part)
  (let ((wordlist (gethash part *buzzwords*)))
    (if wordlist
        (random-elt wordlist)
        (let ((part-defs (remove part grammar :test-not #'eql :key #'first)))
          (if part-defs
              (reduce (lambda (a b) (concatenate 'string a " " b))
                      (mapcar (lambda (el)
                                (cond
                                  ((stringp el)
                                   el)
                                  ((symbolp el)
                                   (buzzword grammar el))
                                  ((eql (first el) '?)
                                   ;; optional
                                   (when (zerop (random 3))
                                     (buzzword grammar (second el))))
                                  (t
                                   (write-to-string el))))
                              (cddr
                               (random-elt part-defs))))
              ;; return just the symbol name if no definitions were found
              (symbol-name part))))))

(defun buzzword-generate (person)
  (buzzword-load)
  (let ((raw-result (buzzword (cons (list 'person '-> person) *grammar*) 'sentence)))
    (string-capitalize
     (string-replace "  " (string-trim " " raw-result) " ")
     :end 1)))

(defcommand manage (message directp target)
  (reply-to message (buzzword-generate target)))