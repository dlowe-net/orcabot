(in-package #:orca)

(defun build-rule-expansions (body)
  "Returns a list of all the possible basic rules that result from the
expansion of the rule body."
  (let ((expansion (and body (build-rule-expansions (rest body)))))
    (cond
      ((null body)
       (list nil))
      ((not (consp (first body)))
       ;; basic clause - just append to all the possible lists
       (mapcar (lambda (expanded)
                 (cons (first body) expanded))
               expansion))
      ((eql (first (first body)) '?)
       ;; optional clause - expands into two bodies - one with, one without
       (mapcan (lambda (expanded)
                   (list*
                    expanded
                    (mapcar
                     (lambda (sub-expansion)
                        (append sub-expansion expanded))
                     (build-rule-expansions (reverse (rest (first body)))))))
               expansion))
      ((eql (first (first body)) 'or)
       ;; alternative clause - expands into n bodies, one of each
       (mapcan (lambda (expanded)
                 (mapcan
                  (lambda (alternative)
                    (if (consp alternative)
                        (mapcan (lambda (sub-expansion)
                                  (list
                                   (append sub-expansion expanded)))
                                (build-rule-expansions (reverse alternative)))
                        (list
                         (cons alternative expanded))))
                  (rest (first body))))
               expansion)))))

(defun expand-grammar (grammar)
  (mapcan
   (lambda (rule)
     (let ((rule-name (first rule))
           (rule-body (cddr rule)))
       (mapcar
        (lambda (new-body)
          (list* rule-name '-> (reverse new-body)))
        (build-rule-expansions (reverse rule-body)))))
   grammar))

(defun load-grammar (path)
  (let ((*package* (find-package "ORCA")))
    (expand-grammar
     (with-open-file (inf path)
       (loop
          for rule = (read inf nil)
          while rule
          do
            (assert (symbolp (first rule)) nil "Invalid rule name ~s before ~d"
                    (first rule)
                    (file-position inf))
            (assert (eql (second rule) '->) nil "Invalid rule ~a before ~d"
                    (first rule)
                    (file-position inf))
            (assert (cddr rule) nil "Empty rule ~a before ~d"
                    (first rule)
                    (file-position inf))
          collect rule)))))

(defun expand-grammar-element (grammar el)
  (cond
    ((stringp el)
     el)
    ((symbolp el)
     (text-from-grammar grammar el))
    ((eql (first el) '?)
     ;; optional
     (if (zerop (random 3))
         (text-from-grammar grammar (second el))
         ""))
    ((eql (first el) 'or)
     (reduce-from-grammar grammar (random-elt (cdr el))))
    (t
     (write-to-string el))))

(defun separate-with-space (prev-el cur-el)
  (and prev-el
       (not (equal prev-el ""))
       (not (equal cur-el ""))
       (let ((prev-char (char prev-el (1- (length prev-el)))))
         (or (alphanumericp prev-char)
             (find prev-char ".?!")))
       (alphanumericp (char cur-el 0))))

(defun reduce-from-grammar (grammar rule-body)
  (with-output-to-string (result)
    (do* ((el-cons rule-body (rest el-cons))
          (prev-el nil expanded-el)
          (expanded-el (expand-grammar-element grammar (first el-cons))
                       (expand-grammar-element grammar (first el-cons))))
         ((null el-cons))
      (when (separate-with-space prev-el expanded-el)
        (princ #\space result))
      (princ expanded-el result))))

(defun text-from-grammar (grammar rule-name)
  (let ((matching-rules (remove rule-name grammar :test-not #'eql :key #'first)))
    (if matching-rules
        (reduce-from-grammar grammar (cddr (random-elt matching-rules)))
        ;; return just the symbol name if no definitions were found
        (symbol-name rule-name))))

(defun grammar-generate (grammar)
  (let ((raw-result (text-from-grammar grammar 'sentence)))
    (concatenate 'string (string-capitalize raw-result :end 1))))

(defcommand manage (message directp &rest target)
  (let ((grammar (load-grammar "/home/dlowe/play/orca/data/manage-grammar.lisp")))
    (reply-to message
              (grammar-generate
               (cons
                `(person -> ,(if target
                                 (format nil "~{~a~^ ~}" target)
                                 "resops"))
                grammar)))))

(defcommand brag (message directp)
  (reply-to message
            (grammar-generate (load-grammar "/home/dlowe/play/orca/data/brag-grammar.lisp"))))