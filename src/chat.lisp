(in-package #:orca)

;; source  person     thing             data
;dlowe: lepore's favorite team is the Red Sox
;dlowe: the red sox is a great baseball team
;("dlowe"   "lepore"   "favorite team"   "the Red Sox")
;("dlowe"   nil        "the red sox"     "a great baseball team")

;; *'s * is * -> (person thing data)
;; * is *     -> (thing data)
;; my * is *  -> (person thing data)
;; what is my * -> query person data
;; your * is * ->
;; what is your
;; what is *'s * -> query person data
;; what is *



(defvar *person* nil)
(defvar *categories* nil)
(defvar *that* nil)
(defvar *topic* nil)
(defvar *responses* nil)
(defvar *inputs* nil)
(defparameter *vars* (make-hash-table :test 'equalp))

(defclass category ()
  ((pattern :accessor pattern-of :initarg :pattern)
   (arity :accessor arity-of :initarg :arity)
   (func :accessor func-of :initarg :func)
   (body :accessor body-of :initarg :body)
   (conditions :accessor conditions-of :initarg :conditions)
   (rank :accessor rank-of :initarg :precedence)))

(defmacro var (sym &optional (person '*person*))
  `(gethash (format nil "~(~a/~a~)" ,person ,sym) *vars* ""))
(defmacro person (sym)
  `(string-capitalize ,sym))

(defun remove-space-runs (string)
  (loop for last-char = nil then c
        for c across string
        unless (and (eql last-char #\space)
                    (eql c #\space))
        collect c into result
        finally (return (coerce result 'string))))

(defun filter-string (string)
  (flet ((valid-char-p (c)
           (or (alpha-char-p c)
               (member c '(#\space #\')))))
    (setf string (remove-if-not #'valid-char-p string))
    (setf string (remove-space-runs string))
    (setf string (string-downcase string))
    (setf string (cl-ppcre:regex-replace-all "'s" string " is"))
    (setf string (cl-ppcre:regex-replace-all "i'm" string "i am"))
    (setf string (cl-ppcre:regex-replace-all "can't" string "can not"))
    (setf string (cl-ppcre:regex-replace-all "cannot" string "can not"))
    (setf string (cl-ppcre:regex-replace-all "n't" string " not"))
    (setf string (cl-ppcre:regex-replace-all "it's" string "it is"))
    (setf string (cl-ppcre:regex-replace-all "'re" string " are"))
    (string-trim '(#\space) string)))

(defun string-replace (needle haystack replacement)
  "Returns a copy of HAYSTACK with all instances of NEEDLE replaced by REPLACMENT.  NEEDLE must be a string of at least one character."
  (assert (plusp (length needle)) nil "string-replace called with zero-length search pattern!")
  (with-output-to-string (result)
    (loop
       with needle-length = (length needle)
       for left = 0 then (+ right needle-length)
       as right = (search needle haystack :start2 left)
       while right
       do (write-string haystack result :start left :end right)
       (write-string replacement result)
       finally (write-string haystack result :start left))))

(defun translate-category-directives (block-sym directive)
  (let ((command (first directive))
        (arguments (rest directive)))
    (case command
      (that
       `(unless (match ,(first arguments) *that*)
         (return-from ,block-sym nil)))
      (topic
       `(unless (match ,(first arguments) *topic*)
         (return-from ,block-sym nil)))
      (when
       `(unless ,(first arguments)
         (return-from ,block-sym nil)))
      (unless
       `(when ,(first arguments)
         (return-from ,block-sym nil)))
      (reduce
       `(push (filter-string (do-format nil ,(first arguments) ,@(rest arguments))) *inputs*))
      (set
       `(setf (gethash (format nil "~(~a/~a~)"
                               ,(or (third arguments) '*person*)
                               ,(first arguments))
                       *vars*)
              ,(second arguments)))
      (setq
       `(setf (gethash (format nil "~(~a/~a~)"
                               ,(or (third arguments) '*person*)
                               ',(first arguments))
                       *vars*)
              ,(second arguments)))
      (say
       (let ((response-sym (gensym "RESPONSE")))
         `(let ((,response-sym (do-format t ,(first arguments)
                                 ,@(rest arguments))))
           (setf *that* (filter-string ,response-sym))
           (push ,response-sym *responses*))))
      (do
       `(progn
          ,@arguments))
      (randomly
       `(case (random ,(length arguments))
         ,@(loop for arg in arguments
                 as idx from 0
                 collect (list idx
                               (translate-category-directives block-sym arg)))))
      (t
       (error "Invalid category directive")))))

(defun category< (a b)
  (cond
    ((not (eql (string= "*" (pattern-of a))
               (string= "*" (pattern-of b))))
     (string= "*" (pattern-of b)))
    ((/= (count 'topic (conditions-of a) :key #'first)
         (count 'topic (conditions-of b) :key #'first))
     ;; Topic clause is most specific
     (> (count 'topic (conditions-of a) :key #'first)
        (count 'topic (conditions-of b) :key #'first)))
    ((/= (count 'that (conditions-of a) :key #'first)
         (count 'that (conditions-of b) :key #'first))
     ;; That clause
     (> (count 'that (conditions-of a) :key #'first)
        (count 'that (conditions-of b) :key #'first)))
    ((/= (count #\* (pattern-of a)) (count #\* (pattern-of b)))
     ;; Wildcards
     (< (count #\* (pattern-of a)) (count #\* (pattern-of b))))
    ((/= (count #\space (pattern-of a)) (count #\space (pattern-of b)))
     ;; Words
     (> (count #\space (pattern-of a)) (count #\space (pattern-of b))))
    (t
     (string< (pattern-of a) (pattern-of b)))))

(defun category-rank (category)
  ;; Alphabetical
  (+ (char-code (char (pattern-of category) 0))
     ;; Topic clause is most specific
     (if (position 'topic (body-of category) :key #'first) 0 256)
     ;; That clause is most specific
     (if (position 'that (body-of category) :key #'first) 0 512)
     ;; Clause with wildcard is less specific
     (if (position #\* (pattern-of category)) 1024 0)
     ;; Complete wildcard should be last
     (if (string= (pattern-of category) "*") 2048 0)))

(defun extract-conditions (body)
  (remove-if-not (lambda (x)
                   (member (first x)
                           '(topic that when unless)))
                 body))

(defun find-identical-category (pattern body)
  (find-if (lambda (x)
             (and (string-equal pattern (pattern-of x))
                  (equal (extract-conditions body)
                         (conditions-of x))))
           *categories*))

(defun category-to-lambda (expr)
  (let ((block-sym (gensym)))
    `(lambda ,(rest (second expr))
       (declare (ignorable ,@(rest (second expr))))
       (block ,block-sym
         ,@(mapcar (lambda (x)
                     (translate-category-directives block-sym x))
                   (cddr expr))
         t))))

(defun add-new-category (expr)
    (setf *categories*
          (sort
           (cons
            (make-instance 'category
                           :pattern (first (second expr))
                           :arity (length (rest (second expr)))
                           :conditions (extract-conditions (cddr expr))
                           :body (cddr expr)
                           :func (compile nil (category-to-lambda expr)))
            *categories*)
           'category<)))

(defun load-chat-categories (path)
  (let ((*package* (find-package "ORCA")))
    (with-open-file (inf path)
      (loop
         for expr = (read inf nil)
         while expr do
           (ecase (first expr)
             (category
              ;; Remove the identical category if one already exists
              (let ((old-category (find-identical-category (first (second expr))
                                                           (cddr expr))))
                (when old-category
                  (setf *categories* (delete old-category *categories*))))
              (add-new-category expr)))))))

(defun switch-person (str)
  (cl-ppcre:regex-replace-all "\\b(mine|me|my|I am|I'm|I|you are|you're|yours|your|you)\\b" str
                              (lambda (target start end match-start match-end reg-starts reg-ends)
                                (declare (ignore start end reg-starts reg-ends))
                                (let ((match (make-array (list (- match-end match-start)) :element-type 'character :displaced-to target :displaced-index-offset match-start)))
                                (cond
                                  ((string= "I" match)
                                   "you")
                                  ((string= "me" match)
                                   "you")
                                  ((string= "my" match)
                                   "your")
                                  ((string= "I am" match)
                                   "you are")
                                  ((string= "I'm" match)
                                   "you're")
                                  ((string= "mine" match)
                                   "yours")
                                  ((string= "you" match)
                                   "I")
                                  ((string= "your" match)
                                   "my")
                                  ((string= "yours" match)
                                   "mine")
                                  ((string= "you're" match)
                                   "I'm")
                                  ((string= "you are" match)
                                   "I am"))))))

(defun do-format (switch-vars string &rest vars)
  (cond
    ((string= "*" string)
     (if switch-vars
         (switch-person (first vars))
         (first vars)))
    ((null (find #\* string))
     string)
    (t
     (let ((string-parts (cl-ppcre:split "\\*" string)))
       (with-output-to-string (output)
         (do ((string-part string-parts (rest string-part))
              (var vars (rest var)))
             ((null string-part))
           (princ (car string-part) output)
           (when var
             (princ (if switch-vars
                        (switch-person (car var))
                        (car var))
                    output))))))))

(defun match (pattern string)
  (let ((pattern-parts (cl-ppcre:split "\\*" pattern :limit 10))
        (vars nil))
    (cond
      ((< (length string) (length pattern))
       (values nil nil))
      ((= (length pattern-parts) 1)
       ;; Easy case - no wildcards
       (values (string= pattern string) nil))
      ((and (first pattern-parts)
            (not (string= (first pattern-parts) string :end2 (length (first pattern-parts)))))
       ;; Beginning anchor exists and doesn't match
       (values nil nil))
      (t
       ;; Matching with wildcards
       (when (first pattern-parts)
         ;; Strip beginning anchor if exists
         (setf string (subseq string (length (first pattern-parts)))))
       (dolist (pattern-part (rest pattern-parts))
         (if (string= pattern-part "")
             ;; Wildcard on the end - push to vars
             (push string vars)
             ;; Wildcard in the middle - push to vars
             (let ((match-pos (search pattern-part string)))
               (unless match-pos
                 (return-from match (values nil nil)))
               (push (subseq string 0 match-pos) vars)
               (setf string (subseq string
                                    (+ match-pos (length pattern-part)))))))
       (values t (nreverse vars))))))

(defun think ()
  (loop while *inputs*
    for input = (pop *inputs*) do
    (format t "THINKING: considering ~s~%" input)
    (loop for category in *categories*
          as (matched vars) = (multiple-value-list (match (pattern-of category) input))
          as finished = (and matched
                             (progn
                               (format t "THINKING: matched ~s (arity ~a)~%"
                                       (pattern-of category)
                                       (arity-of category))
                               (apply (func-of category)
                                      (if (> (length vars) (arity-of category))
                                          (subseq vars 0 (arity-of category))
                                          vars))))
          until finished)))

(defun string-to-inputs (str)
  "Returns a list of inputs, filtered and separated by sentence terminators."
  (mapcar 'filter-string (cl-ppcre:split "[.?!]" str)))

(defun sentence-split-char-p (c)
  (member c '(#\. #\? #\!)))

(defun learn-chat (message)
  (setf *person* (source message))
  (setf *inputs* (string-to-inputs (second message)))
  (setf *responses* nil)
  (think))

(defun repl-chat (message)
  (setf *person* "dlowe")
  (setf *inputs* (string-to-inputs message))
  (setf *responses* nil)
  (think)
  (dolist (response *responses*)
    (format t "~a~%" response)))

(defun chat (message)
  (setf *person* (source message))
  (setf *inputs* (mapcar 'filter-string
                         (cl-ppcre:split "[.?!]" (second (arguments message)))))
  (setf *responses* nil)
  (think)

  (let ((response (join-string "  " (mapcar (lambda (str)
                                              (string-capitalize str :end 1))
                                            (reverse *responses*)))))
    (if (char= #\# (char (first (arguments message)) 0))
        (irc:privmsg (connection message) (first (arguments message))
                     (format nil "~a: ~a" (source message) response))
        (irc:privmsg (connection message) (source message) response))))
