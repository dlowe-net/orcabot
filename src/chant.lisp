;;; Copyright 2012 Daniel Lowe All Rights Reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package #:orcabot)

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

(defmethod handle-command ((module chant-module) (cmd (eql 'chant))
                           message args)
  "chant - set up a chant!"
  (let* ((source (if (message-target-is-channel-p message)
                     (first (arguments message))
                     (source message)))
         (chant (gethash source (chants-of module))))
    (when chant
      (let ((msg (format nil "~a" (string-upcase chant))))
        (if (char= #\# (char (first (arguments message)) 0))
            (irc:privmsg (connection message) (first (arguments message)) msg)
            (irc:privmsg (connection message) (source message) msg))))))