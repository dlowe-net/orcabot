(in-package #:orcabot)

(define-condition bitcoin-error ()
  ((message :accessor message-of :initarg :message)))

(defmethod print-object ((object bitcoin-error) stream)
  (print-unreadable-object (object stream)
    (format stream "~a" (message-of object))))

(defvar *bitcoin-request-cache* nil)
(defvar *bitcoin-request-expiration* nil)

(defun bitcoin-request (url)
  (let ((now (get-universal-time)))
    (cond
      ((and *bitcoin-request-cache*
            (< now *bitcoin-request-expiration*))
       *bitcoin-request-cache*)
      (t
       (setf *bitcoin-request-expiration* (+ now 60))
       (pushnew '("application" . "json") drakma:*text-content-types* :test #'equal)
       (multiple-value-bind (response status)
           (drakma:http-request url)
         (cond
           ((/= status 200)
            (error 'bitcoin-error :message "Couldn't connect to server"))
           (t
            (let* ((json:*json-identifier-name-to-lisp* #'json:simplified-camel-case-to-lisp)
                   (result (json:decode-json-from-string response)))
              (setf *bitcoin-request-cache* result)))))))))

(defun bitcoin-retrieve-currencies ()
  (let ((ticker (bitcoin-request "http://blockchain.info/ticker")))
    (mapcar (alexandria:compose #'symbol-name #'first) ticker)))

(defun bitcoin-retrieve-currency-info (currency)
  (cond
    ((string= currency "BTC")
     (values 1 1 1 1))
    ((string= currency "MBTC")
     (values 1000 1000 1000 1000))
    ((string= currency "UBTC")
     (values 1000000 1000000 1000000 1000000))
    ((string= currency "SATOSHI")
     (values 100000000 100000000 100000000 100000000))
    (t
     (let* ((ticker (bitcoin-request "http://blockchain.info/ticker"))
            (info (rest (assoc (string-upcase currency)
                               ticker
                               :key #'symbol-name
                               :test #'string=))))
       (unless info
         (error 'bitcoin-error :message (format nil "Currency ~a not supported." currency)))
       (values
        (cdr (assoc :|15M| info))
        (cdr (assoc :last info))
        (cdr (assoc :buy info))
        (cdr (assoc :sell info)))))))

(defun bitcoin-exchange (amount from to)
  (let* ((from-rate (bitcoin-retrieve-currency-info from))
         (to-rate (bitcoin-retrieve-currency-info to)))
    (when (or (null from-rate) (null to-rate))
      (error 'bitcoin-error :message "Currency not supported."))
    (float (* (/ amount (rationalize from-rate)) (rationalize to-rate)))))

(defmodule bitcoin bitcoin-module ("btc"))

(defmethod handle-command ((module bitcoin-module)
                           (cmd (eql 'btc))
                           message args)
  ".btc - list supported currencies
.btc <currency> - show exchange rates on BTC for currency
.btc <amount> BTC - convert amount BTC to USD
.btc <amount> <currency> - convert amount in currency to BTC
.btc <amount> <from currency> <to currency> - convert currencies via BTC
"
  (handler-case
      (cond
        ((null args)
         ;; no arguments - list currencies
         (reply-to message "Available currencies: ~{~a~^ ~}"
                   (bitcoin-retrieve-currencies)))
        ((null (cdr args))
         ;; one argument - get currency info
         (handler-case
             (multiple-value-bind (last buy sell avg)
                 (bitcoin-retrieve-currency-info (first args))
               (reply-to message "~a: last:~a buy:~a sell:~a avg:~a"
                         (string-upcase (first args)) last buy sell avg))
           (bitcoin-error (e)
             (reply-to message "Error: ~a" (message-of e)))))
        ((null (cddr args))
         ;; two arguments - convert $1 BTC to $2 currency, unless $2 is
         ;; BTC, then convert $1 USD to BTC.
         (cond
           ((string-equal (second args) "btc")
            (reply-to message "~a BTC => ~f USD"
                      (first args)
                      (bitcoin-exchange (parse-number (first args)) "BTC" "USD")))
           (t
            (reply-to message "~a ~a => ~f BTC"
                      (first args)
                      (string-upcase (second args))
                      (bitcoin-exchange (parse-number (first args))
                                        (string-upcase (second args))
                                        "BTC")))))
        (t
         ;; three arguments - convert $1 of $2 currency into $3 currency
         (let ((from-currency (string-upcase (second args)))
               (to-currency (string-upcase (third args))))
           (reply-to message "~a ~a => ~f ~a"
                     (first args)
                     from-currency
                     (bitcoin-exchange (parse-number (first args))
                                       from-currency
                                       to-currency)
                     to-currency))))
    (bitcoin-error (e)
      (reply-to message "Error: ~a" (message-of e)))
    (simple-type-error (e)
      (declare (ignore e))
      ;; triggered by floating point overflow in sbcl
      (reply-to message "Error: Amount is completely insane"))
    (parse-error (e)
      (reply-to message "~a." e))))