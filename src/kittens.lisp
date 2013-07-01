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

(defmodule kittens kittens-module ("kittens"))

(defmethod handle-message ((module kittens-module)
                           (message irc:irc-privmsg-message))
  (cond
    ((ppcre:scan (ppcre:create-scanner "^orca[,: ]+.*?$" :case-insensitive-mode t) (second (arguments message)))
     (reply-to message (random-elt '("It is certain."
                                     "My sources say no."
                                     "My sources say yes."
                                     "Most likely not."
                                     "UMMM.. No."
                                     "Without a doubt."
                                     "Very doubtful."
                                     "No."
                                     "Yes."
                                     "I don't care."
                                     "Heck yeah!"))))
    ((ppcre:scan (ppcre:create-scanner "\\bpon(y|ies)\\b" :case-insensitive-mode t) (second (arguments message)))
     (reply-to message "OMG!!! Ponies!"))
    ((ppcre:scan (ppcre:create-scanner "\\bkittens?\\b" :case-insensitive-mode t) (second (arguments message)))
     (reply-to message "*squeal*  Kittens!!!"))
    ((ppcre:scan (ppcre:create-scanner "\\bpupp(y|ies)\\b" :case-insensitive-mode t) (second (arguments message)))
     (reply-to message "AwwWWwwWww... Puppies..."))
    ((ppcre:scan (ppcre:create-scanner "\\bmanatees?\\b" :case-insensitive-mode t) (second (arguments message)))
     (reply-to message "MMMmmmmm... Manatees...")))
  nil)
