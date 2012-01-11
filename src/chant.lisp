(in-package :orca)

(defmodule chant chant-module ("chant")
  (chants :reader chants-of :initform (make-hash-table :test 'equal)))

(defmethod examine-message ((module chant-module)
                            (type (eql 'irc:irc-privmsg-message))
                            message)
  (let* ((text (cl-ppcre:split "\\s+"
                               (remove-if-not (lambda (c)
                                                (or (alphanumericp c)
                                                    (eql #\space c)))
                                              (second (arguments message)))))
         (signifier-pos (or (position "more" text :test 'string-equal)
                            (position "less" text :test 'string-equal)
                            (position "too" text :test 'string-equal)))
         (source (if (message-target-is-channel-p message)
                     (first (arguments message))
                     (source message))))
    (when (and signifier-pos
               (> (length text) (1+ signifier-pos)))
      (setf (gethash source (chants-of module))
            (format nil "~a ~a"
                    (elt text signifier-pos)
                    (elt text (1+ signifier-pos)))))))

(defmethod handle-command ((module chant-module)
                           (type (eql 'chant))
                           message args)
  (let* ((source (if (message-target-is-channel-p message)
                     (first (arguments message))
                     (source message)))
         (chant (gethash source (chants-of module))))
    (when chant
      (let ((msg (format nil "~a" (string-upcase chant))))
        (if (char= #\# (char (first (arguments message)) 0))
            (irc:privmsg (connection message) (first (arguments message)) msg)
            (irc:privmsg (connection message) (source message) msg))))))