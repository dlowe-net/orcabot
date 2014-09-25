(in-package #:orcabot)

(esrap:defrule whitespace
    (+ (or #\space #\tab #\newline))
  (:constant nil))

(esrap:defrule paren-expr
    (and #\( expression #\))
  (:destructure (p1 e p2) (declare (ignore p1 p2)) e))

(esrap:defrule literal-integer
    (+ (digit-char-p character))
  (:function (lambda (s)
               (list (reduce (lambda (a b) (+ (* a 10) b)) s :key #'digit-char-p)))))

(esrap:defrule integer
    (or paren-expr literal-integer))

(esrap:defrule dice-op
    (and (esrap:? integer) (esrap:? whitespace) "d" (esrap:? whitespace) (esrap:? integer))
  (:destructure (i1 w1 d1 w2 i2)
                (declare (ignore w1 d1 w2))
                `(,@(or i1 '(1)) ,@(or i2 '(6)) :dice)))

(esrap:defrule factor
    (or dice-op
        integer))

(esrap:defrule mult-op
    (and term (esrap:? whitespace) "*" (esrap:? whitespace) factor)
  (:destructure (i1 w1 d1 w2 i2)
                (declare (ignore w1 d1 w2))
                `(,@i1 ,@i2 :mult)))

(esrap:defrule div-op
    (and term (esrap:? whitespace) "/" (esrap:? whitespace) factor)
  (:destructure (i1 w1 d1 w2 i2)
                (declare (ignore w1 d1 w2))
                `(,@i2 ,@i1 :div)))

(esrap:defrule term
    (or mult-op div-op factor))

(esrap:defrule add-op
    (and expression (esrap:? whitespace) "+" (esrap:? whitespace) term)
  (:destructure (i1 w1 d1 w2 i2)
                (declare (ignore w1 d1 w2))
                `(,@i1 ,@i2 :add)))

(esrap:defrule sub-op
    (and expression (esrap:? whitespace) "-" (esrap:? whitespace) term)
  (:destructure (i1 w1 d1 w2 i2)
                (declare (ignore w1 d1 w2))
                `(,@i2 ,@i1 :sub)))

(esrap:defrule expression
    (or add-op sub-op term))

(defun parse-calc-expr (str)
  (esrap:parse 'expression str :junk-allowed t))

(defun eval-calc (code)
  (let ((stack nil))
    (dolist (c code)
      (case c
        (:dice
         (let ((dice-size (pop stack))
               (dice-num (pop stack)))
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
        (t
         (push c stack))))
    (pop stack)))

(defmodule calc calc-module ("calc" "roll"))

(defmethod handle-command ((module calc-module)
                           (cmd (eql 'calc))
                           message args)
  ".calc [expression] - evaluate an arithmetic expression."
  (let ((str (join-to-string " " args)))
    (reply-to message "~a: ~a" (source message) (eval-calc (parse-calc-expr str)))))

(defmethod handle-command ((module calc-module)
                           (cmd (eql 'roll))
                           message args)
  ".roll <num>d<num>[<action>] - roll a die for an optional action."
  (let ((str (join-to-string " " args)))
    (multiple-value-bind (code end-pt)
        (parse-calc-expr str)
    (reply-to message "~a rolls ~a~a." (source message) (eval-calc code) (if end-pt (subseq str end-pt) "")))))
