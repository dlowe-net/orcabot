(in-package #:orcabot)

(defparameter +starting-balance+ 100)

(defclass transaction ()
  ((source :reader source-of :initarg :source)
   (dest :reader dest-of :initarg :dest)
   (amount :reader amount-of :initarg :amount)
   (channel :reader channel-of :initarg :channel)
   (source-status :accessor source-status-of :initform :unknown)
   (dest-status :accessor dest-status-of :initform :unknown)))

(defmodule credit credit-module ("credits" "give")
  (balances :accessor balances-of :initform (make-hash-table :test 'equalp))
  (pending :accessor pending-of :initform nil))

(defun balance-for-nick (module nick)
  (gethash (normalize-nick nick)
           (balances-of module)
           100))

(defmethod initialize-module ((module credit-module) config)
  (clrhash (balances-of module))
  (with-open-file (inf (data-path "credits.lisp")
                       :direction :input
                       :if-does-not-exist nil)
    (when inf
      (loop for tuple = (read inf nil)
         while tuple
         do (setf (gethash (first tuple) (balances-of module))
                  (second tuple))))))

(defun save-balances (module)
  (with-open-file (ouf (data-path "credits.lisp")
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (maphash (lambda (k v)
               (write (list k v) :stream ouf)
               (terpri ouf))
             (balances-of module))))

(defun update-pending-txns (module nick status)
  (dolist (txn (pending-of module))
    (when (string= nick (source-of txn))
      (setf (source-status-of txn) status))
    (when (string= nick (dest-of txn))
      (setf (dest-status-of txn) status))))

(defun process-eligible-txns (module)
  (dolist (txn (pending-of module))
    (unless (or (eql (source-status-of txn) :unknown)
                (eql (dest-status-of txn) :unknown))
      (let ((balance (balance-for-nick module (source-of txn))))
        (cond
          ((eql (source-status-of txn) :invalid)
           (irc:privmsg (conn-of module) (source-of txn)
                        "You must be logged into NickServ to transfer credits."))
          ((eql (dest-status-of txn) :invalid)
           (irc:privmsg (conn-of module) (source-of txn)
                        (format nil "~a must be logged into NickServ to receive credits." (dest-of txn))))
          ((< balance (amount-of txn))
           (irc:privmsg (conn-of module) (source-of txn)
                        (format nil "You don't have ~d to give to ~a." (amount-of txn) (dest-of txn))))
          (t
           (decf (gethash (normalize-nick (source-of txn))
                          (balances-of module)
                          100)
                 (amount-of txn))
           (incf (gethash (normalize-nick (dest-of txn))
                          (balances-of module)
                          100)
                 (amount-of txn))
           (irc:privmsg (conn-of module) (channel-of txn)
                        (format nil "~a gives ~d credit~:p to ~a."
                                (source-of txn)
                                (amount-of txn)
                                (dest-of txn)))
           (irc:privmsg (conn-of module) (source-of txn)
                        (format nil "You now have ~d credit~:p." (balance-for-nick module (source-of txn))))
           (irc:privmsg (conn-of module) (dest-of txn)
                        (format nil "~a has given you ~d credit~:p.  You now have ~d credit~:p."
                                (source-of txn)
                                (amount-of txn)
                                (balance-for-nick module (dest-of txn))))
           (irc:privmsg (conn-of module) (dest-of txn)
                        (format nil "You now have ~d credit~:p." (balance-for-nick module (dest-of txn))))))
        (save-balances module))))

  ;; now delete the transactions just processed
  (setf (pending-of module)
        (delete-if (lambda (txn)
                     (not (or (eql (source-status-of txn) :unknown)
                              (eql (dest-status-of txn) :unknown))))
                   (pending-of module))))



(defmethod handle-message ((module credit-module)
                           (message irc:irc-notice-message))
  (when (string= (source message) "NickServ")
    (multiple-value-bind (match regs)
        (ppcre:scan-to-strings "STATUS (\\S+) ([0-3])" (second (arguments message)))
      (when match
        (let ((nick (aref regs 0))
              (status (if (member (aref regs 1) '("2" "3") :test #'string=)
                          :valid
                          :invalid)))
          (update-pending-txns module nick status)
          (process-eligible-txns module))
        t))))


(defmethod handle-command ((module credit-module)
                           (cmd (eql 'give))
                           message args)
  "give <credits> <nick> - transfer your credits to another person"
  (multiple-value-bind (amt target)
      (let ((first-amt (parse-integer (first args) :junk-allowed t)))
        (if first-amt
            (values first-amt (second args))
            (values (parse-integer (second args) :junk-allowed t)
                    (first args))))
    (cond
      ((or (null amt)
           (null target))
       (reply-to message "Usage: .give <credits> <nick>"))
      ((string= (normalize-nick (source message))
                (normalize-nick target))
       (reply-to message "Sure... Okay..."))
      ((zerop amt)
       (reply-to message "Done."))
      ((minusp amt)
       (reply-to message "Ha, ha.  Very funny."))
      
      (t
       (irc:privmsg (conn-of module)
                    "NickServ"
                    (format nil "STATUS ~a ~a" (source message) target))
       (push (make-instance 'transaction
                            :source (source message)
                            :dest target
                            :amount amt
                            :channel (first (arguments message)))
             (pending-of module))))))

(defmethod handle-command ((module credit-module)
                           (cmd (eql 'credits))
                           message args)
  "credits - check your credit balance"
  (irc:privmsg (conn-of module)
               (source message)
               (format nil "You have ~a credit~:p."
                       (balance-for-nick module (source message)))))