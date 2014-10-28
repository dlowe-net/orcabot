(in-package #:orcabot)

(esrap:defrule ws
    (esrap:? (+ (or #\space #\tab #\newline)))
  (:constant nil))

(esrap:defrule paren-expr
    (and #\( ws expression ws #\))
  (:destructure (p1 w1 e w2 p2) (declare (ignore p1 w1 w2 p2)) e))

(esrap:defrule literal-integer
    (and (esrap:? (or #\+ #\-)) (+ (digit-char-p character)))
  (:function (lambda (s)
               (list (reduce (lambda (a b) (+ (* a 10) b)) (second s) :key #'digit-char-p)))))

(defparameter +calc-functions+ '(("abs" 1 :abs)
                                 ("mod" 2 :mod)))

(esrap:defrule fname
    (and (esrap:character-ranges (#\a #\z) (#\A #\Z) #\_)
         (+ (esrap:character-ranges (#\a #\z) (#\A #\Z) (#\0 #\9) #\_)))
  (:text t))

(esrap:defrule fargs
    (esrap:? (and expression (* (and #\, ws expression))))
  (:function (lambda (s)
               (when s
                 (append (mapcar #'third (reverse (second s)))
                         (list (first s)))))))

(esrap:defrule funcall
    (and fname ws #\( ws fargs ws #\))
  (:destructure  (f1 w1 p1 w2 a1 w3 p2)
                 (declare (ignore w1 p1 w2 w3 p2))
                 (let* ((func (assoc f1 +calc-functions+ :test #'string=)))
                   (unless func
                     (error "Function not found: ~a" f1))
                   (unless (= (second func) (length a1))
                     (error "Expected ~d arguments to ~a, got ~a" (second func) f1 (length a1)))
                   `(,@(mapcan #'identity a1) ,(third func)))))

(esrap:defrule integer
    (or paren-expr funcall literal-integer))

(esrap:defrule dice-op
    (and (esrap:? integer) ws "d" ws (esrap:? integer))
  (:destructure (i1 w1 d1 w2 i2)
                (declare (ignore w1 d1 w2))
                `(,@(or i2 '(6)) ,@(or i1 '(1)) :dice)))

(esrap:defrule factor
    (and (esrap:? (and (or "-" "+") ws))
         (or dice-op
             integer))
  (:destructure (n1 f1)
                (if (and n1 (string= (first n1) "-"))
                    `(,@f1 -1 :mult)
                    f1)))

(esrap:defrule factor-op
    (and term ws (or "*" "/" "%") ws factor)
  (:destructure (i1 w1 o1 w2 i2)
                (declare (ignore w1 w2))
                `(,@i2 ,@i1 ,(alexandria:switch (o1 :test #'string=)
                                                ("*" :mult) ("/" :div) ("%" :mod)))))

(esrap:defrule term
    (or factor-op factor))

(esrap:defrule term-op
    (and expression ws (or "+" "-") ws term)
  (:destructure (i1 w1 o1 w2 i2)
                (declare (ignore w1 w2))
                `(,@i2 ,@i1 ,(if (string= o1 "+") :add :sub))))

(esrap:defrule expression
    (or term-op term))

(defun parse-calc-expr (str)
  (esrap:parse 'expression str :junk-allowed t))

(defun eval-calc (result-type code)
  (let ((stack nil))
    (dolist (c code)
      (case c
        (:dice
         (let* ((dice-num (pop stack))
                (dice-size (pop stack)))
           (cond
             ((not (< 0 dice-num 1000))
              (return-from eval-calc (format nil "ERR: Invalid dice rolls ~a." dice-num)))
             ((not (< 0 dice-size 1000))
              (return-from eval-calc (format nil "ERR: Invalid dice size ~a." dice-size)))
             (t
              (push (loop
                       repeat dice-num
                       for roll = (1+ (random dice-size))
                       summing roll)
                    stack)))))
        (:add
         (push (+ (pop stack) (pop stack)) stack))
        (:sub
         (push (- (pop stack) (pop stack)) stack))
        (:mult
         (push (* (pop stack) (pop stack)) stack))
        (:div
         (push (/ (pop stack) (pop stack)) stack))
        (:mod
         (push (mod (pop stack) (pop stack)) stack))
        (:abs
         (push (abs (pop stack)) stack))
        (t
         (push c stack))))
    (funcall
     (case result-type
       (integer
        #'truncate)
       (float
        #'float)
       (rational
        #'rational)
       (t
        #'identity))
     (pop stack))))

(defmodule calc calc-module ("calc" "roll"))

(defun parse-calc-args (raw-args)
  "Taking the raw-args to a calc command, returns (INTP FLOATP CODE FLAVOR)."
  (let (opts args)
    (dolist (arg raw-args)
      (if (and (> (length arg) 1)
               (string= "--" arg :end2 2))
          (push arg opts)
          (push arg args)))
    (let ((expr (join-to-string " " (nreverse args))))
      (multiple-value-bind (code end-pt)
          (parse-calc-expr expr)
        (let* ((flavor-text (if end-pt
                                (subseq expr end-pt)
                                ""))
               (end-char (and (> (length flavor-text) 1)
                              (char flavor-text (1- (length flavor-text)))))
               (flavor (if (member end-char '(#\. #\? #\!))
                           flavor-text
                           (concatenate 'string flavor-text "."))))
          (values
           (cond
             ((find "--int" opts :test #'string=)
              'integer)
             ((find "--float" opts :test #'string=)
              'float)
             ((find "--ratio" opts :test #'string=)
              'rational)
             (t nil))
           expr
           code
           flavor))))))

(defmethod handle-command ((module calc-module)
                           (cmd (eql 'calc))
                           message args)
  ".calc [--int|--float|--ratio] <expression> - evaluate an arithmetic expression."
  (multiple-value-bind (result-type str code flavor)
      (parse-calc-args args)
    (declare (ignore flavor))
    (if code
        (reply-to message "~a: ~a ~@[(~(~a~))~]= ~a"
                  (source message) str result-type
                  (eval-calc result-type code))
        (reply-to message "~a: Parse error." (source message)))))

(defmethod handle-command ((module calc-module)
                           (cmd (eql 'roll))
                           message args)
  ".roll <num>d<num>[<action>] - roll a die for an optional action."
  (multiple-value-bind (result-type str code flavor)
      (parse-calc-args args)
    (declare (ignore str))
    (if code
        (reply-to message "~a rolls ~a~a~a" (source message) (eval-calc result-type code) flavor)
        (reply-to message "~a rolls something funky that I didn't understand." (source message)))))
