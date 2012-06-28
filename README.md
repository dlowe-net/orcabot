Orcabot - A modular IRC bot
===========================

Orcabot is an IRC bot written in Common Lisp, intended to be
functional and easy to maintain.  It was developed using sbcl, so this
documentation assumes that you have sbcl installed.

Orcabot also depends on quicklisp to load the libraries it depends on, and
assumes that quicklisp is loaded in your .sbclrc file.

Orcabot gets all of its configuration from a file placed in the
sessions directory in the project root.  Here is a minimal example session:

    (nick "orcabot")
    (server "irc.example.com" :port 6667)
    (autojoin "#orcabot" "#lisp")
    (modules admin basic chant)
    (access
      (allow :user "me" :modules (admin))
      (deny :modules (admin)))

To start orcabot, running this at the command line should be all that
is required:

    sbcl --load "bin/start.lisp" <session filename>

Each module can be enabled or disabled independently of the others,
and can implement a wide array of features.  A list of modules can be
found within the documentation.
