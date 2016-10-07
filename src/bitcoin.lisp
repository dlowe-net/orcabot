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
       (handler-case
           (multiple-value-bind (response status)
               (drakma:http-request url)
             (cond
               ((/= status 200)
                (error 'bitcoin-error :message "Couldn't connect to server"))
               (t
                (let* ((json:*json-identifier-name-to-lisp* #'json:simplified-camel-case-to-lisp)
                       (result (json:decode-json-from-string response)))
                  (setf *bitcoin-request-cache* result)))))
         (usocket:timeout-error ()
           (error 'bitcoin-error :message "Timed out connecting to server")))))))

(defun retrieve-bitstamp-info ()
  (let* ((info (bitcoin-request "http://bitstamp.net/api/ticker/")))
    (values
     (cdr (assoc :last info))
     (cdr (assoc :high info))
     (cdr (assoc :low info))
     (cdr (assoc :vwap info))
     (cdr (assoc :volume info))
     (cdr (assoc :bid info))
     (cdr (assoc :ask info)))))

(defun retrieve-bitfinex-info ()
  (let ((info (bitcoin-request "https://api.bitfinex.com/v1/pubticker/btcusd")))
    (values
     (cdr (assoc :last_price info))
     (cdr (assoc :high info))
     (cdr (assoc :low info))
     (cdr (assoc :mid info))
     (cdr (assoc :volume info))
     (cdr (assoc :bid info))
     (cdr (assoc :ask info)))))

(defmodule bitcoin bitcoin-module ("btc"))

(defmethod handle-command ((module bitcoin-module)
                           (cmd (eql 'btc))
                           message args)
  ".btc - show bitcoin trading information"
  (handler-case
      (multiple-value-bind (last buy sell avg vol bid ask)
          (retrieve-bitstamp-info)
        (reply-to message "last:~a high:~a low:~a avg:~a volume:~a bid:~a ask:~a"
                  last buy sell avg vol bid ask))
    (bitcoin-error (e)
      (reply-to message "Error: ~a" (message-of e)))))
