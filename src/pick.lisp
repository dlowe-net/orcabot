(in-package #:orcabot)

(defun save-pick-catalog (module)
  (with-open-file (ouf (data-path "pick-catalog.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write (catalog-of module) :stream ouf)))

(defun load-pick-catalog (module)
  (with-open-file (inf (data-path "pick-catalog.lisp")
                       :if-does-not-exist nil)
    (setf (catalog-of module) (if inf (read inf nil) nil))))

(defmodule pick pick-module ("pick")
  (catalog :accessor catalog-of :initform nil))

(defmethod initialize-module ((module pick-module) config)
  (load-pick-catalog module))

(defun lookup-category (catalog category)
  (rest (assoc category catalog :test #'string-equal)))

(defun find-reference-cycles (catalog start category)
  (dolist (term (lookup-category catalog category))
    (when (or (string-equal start term)
              (find-reference-cycles catalog start term))
      (return t))))

(defun add-pick-choice (module message category choices)
  (let ((bad-choices (remove-if-not (lambda (choice)
                                      (find-reference-cycles (catalog-of module) category choice))
                                    choices)))
    (when bad-choices
      (reply-to message "Ignoring choices that would cause a dependency cycle: ~{~a~^, ~}"
                bad-choices))
    (setf choices (set-difference choices bad-choices)))

  (let ((tuple (assoc category (catalog-of module) :test #'string-equal)))
    (cond
      ((null choices)
       (reply-to message "No choices to add."))
      ((null tuple)
       (push (cons category choices) (catalog-of module))
       (save-pick-catalog module)
       (reply-to message "Category '~a' added with choices: ~{~a~^, ~}"
                 category
                 choices))
      (t
       (let* ((new-choices (set-difference choices (rest tuple) :test #'string=)))
         (cond
           (new-choices
            (setf (rest tuple) (append (rest tuple) new-choices))
            (save-pick-catalog module)
            (reply-to message "Added ~{~a~^, ~} to category '~a'" new-choices category))
           (t
            (reply-to message "No new choices added to category '~a'" category))))))))

(defun del-pick-choice (module message category choices)
  (cond
    ((null (assoc category
                  (catalog-of module)
                  :test #'string-equal))
     (reply-to message "Category '~a' does not exist." category))
    ((null choices)
     (let ((category-def (assoc category
                                (catalog-of module)
                                :test #'string-equal)))
       (setf (catalog-of module) (delete category-def (catalog-of module)))
       (save-pick-catalog module)
       (reply-to message "Category '~a': removed ~@[(~{~a~^, ~})~]"
                 (first category-def)
                 (rest category-def))))
    (t
     (let* ((category-def (assoc category
                                 (catalog-of module)
                                 :test #'string-equal))
            (doomed-picks (intersection choices
                                        (rest category-def)
                                        :test #'string-equal))
            (invalid-picks (set-difference choices
                                           (rest category-def)
                                           :test #'string-equal)))
       ;; Remove category if all picks are removed
       (cond
         ((subsetp (rest category-def) doomed-picks :test #'string-equal)
          (setf (catalog-of module) (delete category-def (catalog-of module)))
          (reply-to message "Category '~a': removed ~@[ (~{~a~^, ~})~]"
                    (first category-def)
                    (rest category-def)))
         (t
          (setf (rest category-def) (set-difference (rest category-def)
                                                    doomed-picks
                                                    :test #'string-equal))
          (reply-to message "Category '~a':~@[ removed ~{~a~^, ~}~]~:[~;, ~]~@[ ignored ~{~a~^, ~}~]"
                    (first category-def)
                    doomed-picks
                    (and doomed-picks invalid-picks)
                    invalid-picks)))

       (save-pick-catalog module)))))

(defun parse-pick-args (raw-args)
  (let (opts args)
    (dolist (arg raw-args)
      (if (string= "--" arg :end2 2)
          (push (string-left-trim "-" arg) opts)
          (push arg args)))
    (setf args (nreverse args))
    (let* ((addp (find "add" opts :test #'string=))
           (delp (find "del" opts :test #'string=))
           (showp (find "show" opts :test #'string=))
           (category (when (or addp delp showp)
                       (pop args))))
      (values
       addp delp showp category
       (re:split "\\s*,\\s*" (join-to-string " " args))))))

(defmethod handle-command ((module pick-module)
                           (cmd (eql 'pick))
                           message args)
  ".pick <choice>, <choice> [,<choice>...] ) - selects randomly between choices
.pick - lists categories from which to pick
.pick <category> - selects random option in category
.pick --add category <choice> - adds option to category
.pick --del category <choice> - removes option from category
"
  (multiple-value-bind (addp delp showp category choices)
      (parse-pick-args args)
    (cond
     ((and category (find #\, category))
      (reply-to message "~a: Categories may not have commas in their names." (source message)))
     (addp
      (add-pick-choice module message category choices))
     (delp
      (del-pick-choice module message category choices))
     (showp
      (if category
          (let ((terms (lookup-category (catalog-of module) category)))
            (if terms
                (reply-to message "Picks in category ~a: ~{~a~^, ~}" category terms)
                (reply-to message "~a is not a category." category)))
          (reply-to message "Pick categories: ~{~a~^, ~}"
                    (mapcar #'first (catalog-of module)))))
     ((endp choices)
      (reply-to message "~a: Pick out of what choices?" (source message)))
     ((cdr choices)
      (reply-to message "~a: I pick ~a!" (source message) (random-elt choices)))
     ((lookup-category (catalog-of module) (first choices))
      (let ((choice (random-elt (lookup-category (catalog-of module) (first choices)))))
        (loop for terms = (lookup-category (catalog-of module) choice)
           while terms do
             (setf choice (random-elt terms)))
        (reply-to message "~a: I pick ~a!" (source message) choice)))
     (t
      (reply-to message "~a is not a category." (first choices))))))
