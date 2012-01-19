(defpackage #:orca-system (:use #:asdf #:cl))
(in-package #:orca-system)

#.(declaim (optimize (debug 3) (speed 0) (safety 3) (space 2)))

(defsystem :orca
  :name "Orca"
  :version "2.0.0"
  :author "Daniel Lowe <dlowe@bitmuse.com>"
  :description "Orca IRC bot"
  :depends-on (cl+ssl cl-irc local-time cl-ppcre html-entities drakma cxml)

  :components
  ((:module :src :components
            ((:file "defpackage")
             (:file "utils" :depends-on ("defpackage"))
             (:file "module" :depends-on ("utils"))
             (:file "admin" :depends-on ("module"))
             (:file "basic" :depends-on ("module"))
             (:file "chant" :depends-on ("module"))
             (:file "env" :depends-on ("module"))
             (:file "grammar" :depends-on ("module"))
             (:file "stats" :depends-on ("module"))
             (:file "itatix" :depends-on ("module"))
             (:file "itasvn" :depends-on ("module"))
             (:file "itabug" :depends-on ("module"))
             (:file "trivia" :depends-on ("module"))
             (:file "main" :depends-on ("module"))))))