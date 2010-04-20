(defpackage #:orca-system (:use #:asdf #:cl))
(in-package #:orca-system)

#.(declaim (optimize (debug 3) (speed 0) (safety 3) (space 2)))

(defsystem :orca
  :name "Orca"
  :version "1.0.0d"
  :author "Daniel Lowe <dlowe@bitmuse.com>"
  :description "Orca IRC bot"
  :depends-on (cl+ssl cl-irc local-time cl-ppcre html-entities drakma
                      hunchentoot)

  :components
  ((:module :src :components
            ((:file "defpackage")
             (:file "utils" :depends-on ("defpackage"))
             (:file "db" :depends-on ("defpackage" "utils"))
             (:file "parrot" :depends-on ("defpackage" "utils"))
             (:file "chant" :depends-on ("defpackage" "utils"))
             (:file "svn" :depends-on ("defpackage" "utils"))
             (:file "pounder" :depends-on ("defpackage" "utils"))
             (:file "lol" :depends-on ("defpackage" "utils"))
             (:file "chat" :depends-on ("defpackage" "utils"))
             (:file "bug" :depends-on ("defpackage" "utils"))
             (:file "tix" :depends-on ("defpackage" "utils"))
             (:file "pastebin" :depends-on ("defpackage" "utils"))
             (:file "main"
                    :depends-on ("defpackage" "utils" "db" "bug" "tix"
                                              "svn" "pounder" "chat" "lol"
                                              "pastebin"
                                              "chant" "parrot"))))))