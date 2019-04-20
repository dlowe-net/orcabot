(in-package #:orcabot)

;;; utilities that only deal with strings

(defun join-to-string (delimiter seq)
  "Returns a string with the printed elements of SEQ seperated by the
printed elements of DELIMITER."
  ;; quick check for zero-length list, avoiding the call to LENGTH
  (when (endp seq)
    (return-from join-to-string ""))
  
  (let ((seq-len (length seq)))
    ;; quick check for zero-length sequence before we do anything else
    (when (zerop seq-len)
      (return-from join-to-string ""))

    (let* ((del-len (length delimiter))
           (total-seq-len (reduce #'+ seq :key #'length :initial-value 0))
           (result (make-string (+ total-seq-len
                                   (* (1- seq-len) del-len)))))
      (cond
        ((consp seq)
         (loop
           with offset = 0
           for el-cons on seq
           as el = (car el-cons)
           do
              (replace result el :start1 offset)
              (incf offset (length el))
              (when (cdr el-cons)
                (replace result delimiter :start1 offset)
                (incf offset del-len)))
         result)
        (t
         (loop
           with offset = 0
           for idx from 0 upto (- seq-len 2)
           as el = (elt seq idx)
           do
              (replace result el :start1 offset)
              (incf offset (length el))
              (replace result delimiter :start1 offset)
              (incf offset del-len)
           finally (replace result (elt seq (1- seq-len)) :start1 offset))
         result)))))

(defun strip-html (str)
  (ppcre:regex-replace-all "<.*?>" str ""))

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

(defun wrap-string (string width)
  "Given an initial string, returns the string broken up into lines breaking on word boundaries."
  (loop
     for raw-wrap-pos = width then (+ wrap-pos width)
     until (> raw-wrap-pos (length string))
     for wrap-pos = (or (position-if-not #'alpha-char-p string :end raw-wrap-pos :from-end t)
                        raw-wrap-pos)
     for start-pos = 0 then (1+ wrap-pos)
     collect (string-trim " " (subseq string start-pos wrap-pos))
     into result
     finally (return (nconc result
                            (list (string-trim " "
                                               (subseq string (or wrap-pos 0))))))))

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
     (alexandria:switch (target :test (lambda (a b)
                                        (string-equal a b
                                                      :start1 match-start
                                                      :end1 match-end)))
       ("I" "you")
       ("me" "you")
       ("my" "your")
       ("I am" "you are")
       ("I'm" "you're")
       ("mine" "yours")
       ("you" "I")
       ("you" "me")
       ("your" "my")
       ("you are" "I am")
       ("you're" "I'm")
       ("yours" "mine")))))

(define-condition unknown-option (parse-error)
  ((option :initarg :option)))

(define-condition unexpected-argument-end (parse-error)
  ((option :initarg :option)))

(define-condition invalid-option (parse-error)
  ((option :initarg :option)
   (bad-value :initarg :bad-value)))

(define-condition invalid-option-type (error)
  ((option :initarg :option)))

(defun extract-option (arg)
  "Parses strings of form \"foo\", \"--foo\", and \"--foo=bar\".  Returns three values - the first is T when the string is an option, otherwise NIL.  The second value is the option name, the third is the value of the option, or NIL if the value was not specified."
  (let ((raw-option (when (and (string/= "" arg)
                               (member (char arg 0) '(#\- #\em_dash)))
                      (string-left-trim '(#\- #\em_dash) arg))))
    (unless raw-option
      (return-from extract-option nil))

    (let* ((equals-pos (position #\= raw-option))
           (opt-name (if equals-pos
                         (subseq raw-option 0 equals-pos)
                         raw-option))
           (opt-value (if equals-pos
                          (subseq raw-option (1+ equals-pos))
                          nil)))
      (values t opt-name opt-value))))

(defun parse-args (spec raw-args)
  "Given a spec in the format ((OPTION . TYPE) ...), parses a list of
arguments and returns two values: a property list of the option values
and a list of the positional arguments.  May raise conditions
UNKNOWN-OPTION, UNEXPECTED-ARGUMENT-END, or INVALID-OPTION-TYPE."
  (let ((opts nil)
        (args nil))
    (labels ((string-value (arg)
               (when (null arg)
                 (error 'unexpected-argument-end
                        :option (first opts)))
               (push arg opts)
               #'option-arg-or-end)
             (integer-value (arg)
               (when (null arg)
                 (error 'unexpected-argument-end
                        :option (first opts)))
               (handler-case
                   (push (parse-integer arg) opts)
                 (parse-error (err)
                   (declare (ignore err))
                   (error 'invalid-option
                          :option (first opts)
                          :bad-value arg)))
               #'option-arg-or-end)
             (arg-or-end (arg)
               (unless (null arg)
                 (push arg args)
                 #'arg-or-end))
             (option-arg-or-end (arg)
               (unless (null arg)
                 (multiple-value-bind (optionp opt-name opt-value)
                     (extract-option arg)
                   (cond
                     ((null optionp)
                      (push arg args)
                      #'option-arg-or-end)
                     ((string= opt-name "")
                      #'arg-or-end)
                     (t
                      (let ((opt-spec (assoc opt-name spec :key #'symbol-name :test #'string-equal)))
                        (when (null opt-spec)
                          (signal 'unknown-option :option arg))
                        (push (car opt-spec) opts)
                        (case (cdr opt-spec)
                          (boolean
                           (push t opts)
                           #'option-arg-or-end)
                          (string
                           (if opt-value
                               (string-value opt-value)
                               #'string-value))
                          (integer
                           (if opt-value
                               (integer-value opt-value)
                               #'integer-value))
                          (t
                           (error 'invalid-option-type
                                  :option (car opt-spec)))))))))))
      (loop
         with expect = #'option-arg-or-end
         until (null expect)
         do
           (setf expect (funcall expect (pop raw-args)))))
    (values (reverse opts) (reverse args))))
