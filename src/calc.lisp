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

(esrap:defrule integer
    (or paren-expr literal-integer))

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
    (and term ws (or "*" "/") ws factor)
  (:destructure (i1 w1 o1 w2 i2)
                (declare (ignore w1 w2))
                `(,@i2 ,@i1 ,(if (string= o1 "*") :mult :div))))

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

(defun eval-calc (code)
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
        (t
         (push c stack))))
    (pop stack)))

(defmodule calc calc-module ("calc" "roll"))

(defmethod handle-command ((module calc-module)
                           (cmd (eql 'calc))
                           message args)
  ".calc [expression] - evaluate an arithmetic expression."
  (let ((str (join-to-string " " args)))
    (let ((code (parse-calc-expr str)))
      (if code
          (reply-to message "~a: ~a" (source message) (eval-calc code))
          (reply-to message "~a: Parse error." (source message))))))

(defmethod handle-command ((module calc-module)
                           (cmd (eql 'roll))
                           message args)
  ".roll <num>d<num>[<action>] - roll a die for an optional action."
  (let ((str (join-to-string " " args)))
    (multiple-value-bind (code end-pt)
        (parse-calc-expr str)
      (if code
          (reply-to message "~a rolls ~a~a." (source message) (eval-calc code) (if end-pt (subseq str end-pt) ""))
          (reply-to message "~a rolls something funky that I didn't understand." (source message))))))
