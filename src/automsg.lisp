;;; Copyright 2019 Daniel Lowe All Rights Reserved.
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

(defmodule automsg automsg-module ()
  (messages))

(defmethod initialize-module ((module automsg-module) config)
  (with-slots (messages) module
    (setf messages (rest (assoc 'automsg config)))))

(defmethod examine-message ((module automsg-module)
                            (message irc:irc-join-message))
  (with-slots (messages) module
    (let ((automsg (cdr (assoc (first (arguments message)) messages :test 'equal))))
      (when automsg
        (irc:privmsg (connection message) (first (arguments message)) automsg)))))
