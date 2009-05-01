(defpackage #:orca-system (:use #:asdf #:cl))
(in-package #:orca-system)

#.(declaim (optimize (debug 3) (speed 0) (safety 3) (space 2)))

(defsystem :orca
  :name "Orca"
  :version "1.0.0d"
  :author "Daniel Lowe <dlowe@bitmuse.com>"
  :description "Orca IRC bot"
  :depends-on (cl+ssl cl-irc local-time cl-ppcre drakma)

  :components
  ((:module :src :components
            ((:file "defpackage")
             (:file "utils" :depends-on ("defpackage"))
             (:file "db" :depends-on ("defpackage" "utils"))
             (:file "parrot" :depends-on ("defpackage" "utils"))
             (:file "main" :depends-on ("defpackage" "utils" "db" "parrot"))))))