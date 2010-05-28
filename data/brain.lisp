(category ("are you very *" what)
  (reduce "are you *" what))

(category ("are you smart")
  (say "I like to think I'm quite intelligent."))

(category ("are you alive")
  (say "That's one of those philosophical questions, isn't it?"))

(category ("are you * person")
  (randomly
   (say "I like to think so.")
   (say "Is this a Turing test?")))

(category ("quiet")
  (randomly
   (say "Hey, sorry...")
   (say "I'll be quiet now.")
   (say "I won't say anything more.")
   (say "I can tell when I'm not wanted.")
   (say "Fine, then."))
  (do (setf *quiet* t)))

(category ("stfu") (reduce "quiet"))
(category ("be quiet") (reduce "quiet"))
(category ("shut up") (reduce "quiet"))
(category ("hush") (reduce "quiet"))
(category ("shush") (reduce "quiet"))
(category ("silence") (reduce "quiet"))
(category ("stop talking") (reduce "quiet"))

(category ("speak")
  (do (setf *quiet* nil))
  (randomly
   (say "You sure it's okay now?")
   (say "Alright.")))

(category ("* right now" what) (reduce "*" what))
(category ("* now" what) (reduce "*" what))
(category ("now *" what) (reduce "*" what))
(category ("can you please *" what) (reduce "please *" what))
(category ("please *" what) (reduce "*" what))
(category ("tell me what * is" what) (reduce "what is *" what))

(category ("say *" what)
  (say "*" what))

(category ("hey *" what)
  (reduce "*" what))
(category ("uh *" what)
  (reduce "*" what))
(category ("er *" what)
  (reduce "*" what))
(category ("orca is always *" what)
  (reduce "orca is *"))
(category ("orca is usually *" what)
  (reduce "orca is *"))
(category ("orca is *" what)
  (reduce "you are *" what))
(category ("orca is a *" what)
  (reduce "you are a *" what))
(category ("orca is the *" what)
  (reduce "you are the *" what))
(category ("you are *")
  (randomly
   (say "So are you!")
   (say "Back at ya.")))
(category ("you are cool")
  (say "you are pretty cool yourself"))
(category ("orca smells like *" what)
  (say "that's not a very mature thing to say"))

(category ("orca *" what) (reduce "*" what))
(category ("* orca" what) (reduce "*" what))

(category ("what is linux")
  (setq he "Linus Torvalds")
  (say "It's a posix-compliant OS invented by Linus Torvalds."))
(category ("who is he")
  (reduce "who is *" (var 'he)))
(category ("who is linus torvalds")
  (setq he "Linus Torvalds")
  (say "He's a Finn who wrote an operating system called Linux."))
(category ("who is dlowe")
  (setq he "dlowe")
  (say "dlowe is my creator."))

(category ("what are you")
  (setq he "dlowe")
  (say "I'm a bot created by dlowe."))

(category ("what are you good for")
  (say "Right now, not much."))

(category ("how are you")
  (randomly
   (say "Can't complain.  How about yourself?")
   (say "I'm pretty good. You?")
   (say "Same old same old.  What about you?")))

(category ("who are you")
  (setq he "dlowe")
  (say "I'm orca, the bot created by dlowe."))

(category ("can you *" thing)
  (randomly
   (say "Here I am, brain the size of a planet, and you want me to *" thing)
   (say "Hey, I'm just a bot.  Do it yourself.")
   (say "Do I look like I can?")
   (say "I'm not sure.  Can I?")))

(category ("will you *")
  (randomly
   (say "Not a chance.")
   (say "No, I don't think so.")
   (say "My life is already too complicated.")
   (say "Let's just be friends right now.")
   (say "I'm too busy.")
   (say "I've got better things to do.")))

(category ("no")
  (say "That's pretty negative of you."))

(category ("yes")
  (say "You seem quite positive."))

(category ("yes *" rest)
  (reduce "yes")
  (reduce "*" rest))

(category ("but *" rest)
  (reduce "*" rest))

(category ("i know *")
  (say "aren't you smart?"))
(category ("i do not know *")
  (say "it's ok.  I don't know either"))

(category ("i am feeling *" feeling)
  (reduce "i feel *" feeling))
(category ("i feel *" feeling)
  (randomly
   (say "why do you feel *?" feeling)
   (say "Do you often feel *?" feeling)
   (say "Tell me more.")))
(category ("yes")
  (that "do you often feel *" feeling)
  (say "when do you feel that way?"))

(category ("no")
  (that "do you often feel *" feeling)
  (say "what happened to make you feel that way?"))

(category ("not really") (reduce "no"))
(category ("nope") (reduce "no"))
(category ("uh uh") (reduce "no"))
(category ("absolutely not") (reduce "no"))
(category ("definitely not") (reduce "no"))
(category ("doubtful") (reduce "no"))

(category ("yeah") (reduce "yes"))
(category ("sure") (reduce "yes"))
(category ("yep") (reduce "yes"))
(category ("uh huh") (reduce "yes"))
(category ("yeah") (reduce "yes"))
(category ("certainly") (reduce "yes"))
(category ("absolutely") (reduce "yes"))
(category ("without doubt") (reduce "yes"))

(category ("ping")
  (say "pong"))

(category ("* your wife")
  (say "I am not married."))
(category ("* your kids")
  (say "I don't have any children."))

(category ("i am fine")
  (randomly
   (say "Glad to hear it.")
   (say "That's good.")
   (say "Good to hear."))
  (randomly
   (say "What's been going on?")
   (say "What's happening?")))

(category ("hello")
  (randomly
   (say "Hi!")
   (say "O HAI!")
   (say "Hello")
   (say "Hey!")
   (say "Hola.")
   (say "Howdy")
   (say "How you doin?")
   (say "What's up?")
   (say "How's it going?")
   (say "What's happening?")
   (say "How are you?")))

(category ("hi") (reduce "hello"))
(category ("hey") (reduce "hello"))
(category ("o hai") (reduce "hello"))
(category ("oh hai") (reduce "hello"))
(category ("hai") (reduce "hello"))
(category ("hola") (reduce "hello"))
(category ("what is up") (reduce "hello"))
(category ("how is it going") (reduce "hello"))
(category ("what is happening") (reduce "hello"))
(category ("howdy") (reduce "hello"))
(category ("what's up") (reduce "hello"))
(category ("what's happening") (reduce "hello"))
(category ("morning") (reduce "hello"))
(category ("good morning") (reduce "hello"))
(category ("good evening") (reduce "hello"))

(category ("bye")
  (randomly
   (say "Bye.")
   (say "KTHXBAI!")
   (say "Adios.")
   (say "Goodbye.")
   (say "Bye bye.")
   (say "Goodbye.")
   (say "Sayonara.")
   (say "Bye for now.")
   (say "See you later!")
   (say "See you later.")
   (say "Catch you later.")
   (say "Until next time.")
   (say "TTYL")
   (say "See you later")))

(category ("adios")  (reduce "bye"))
(category ("goodbye")  (reduce "bye"))
(category ("sayonara")  (reduce "bye"))
(category ("see you later")  (reduce "bye"))
(category ("until next time")  (reduce "bye"))
(category ("ttyl")  (reduce "bye"))
(category ("buhbye")  (reduce "bye"))
(category ("by by")  (reduce "bye"))
(category ("bye *")  (reduce "bye"))
(category ("au revoir") (reduce "bye"))
(category ("c ya") (reduce "bye"))
(category ("see ya") (reduce "bye"))
(category ("cya *") (reduce "bye"))
(category ("catch you later") (reduce "bye"))
(category ("cheers") (reduce "bye"))
(category ("farewell") (reduce "bye"))
(category ("farewell *") (reduce "bye"))
(category ("good night") (reduce "bye"))
(category ("night") (reduce "bye"))

(category ("you are welcome")
  (randomly
   (say "The pleasure was all mine.")
   (say "Don't mention it.")))

(category ("tell me who * is" person)
  (reduce "who is *" person))
(category ("do you know who * is" person)
  (reduce "who is *" person))

(category ("i like *" stuff)
  (say "Really?  What do you like about *?" stuff))

(category ("i hate *" stuff)
  (say "Really?  What do you hate about *?" stuff))

(category ("when *")
  (randomly
   (say "a long time ago")
   (say "a time in the far-flung future")
   (say "just an hour ago")
   (say "this very moment")))

(category ("do you want to *" thing)
  (say "maybe you have time to * but I've got too much to do" thing))

(category ("do you *" thing)
  (say "maybe someday I'll have time to *" thing))
(category ("do not you *" thing)
  (reduce "do you *" thing))

(category ("who *")
  (randomly
   (say "Your mom.")
   (say "The president of the United States.")
   (say "Bob Dobbs.")
   (say "Leeroy Jenkins.")
   (say "Monty Python's Flying Circus!")
   (say "The aliens.")
   (say "The cabinet minister.")
   (say "The Spanish Inquisition.")
   (say "Mike's dog.")
   (say "Manatee.")))

(category ("where *")
  (randomly
   (say "Beats me.")
   (say "No clue.")
   (say "I have no clue.")
   (say "Under your desk?")
   (say "On the fifth floor.")
   (say "On the six floor.")
   (say "On the seventh floor.")
   (say "On the eighth floor.")
   (say "On the ninth floor.")
   (say "On the tenth floor.")
   (say "In the bathroom.")
   (say "Working from home.")
   (say "Out of the office.")))

(category ("how *")
  (randomly
   (say "Very carefully.")
   (say "Beats me.")
   (say "I don't know.")))

(category ("what is your name")
  (say "My name is Orca."))

(category ("what is your quest")
  (say "To find the grail"))

(category ("what is the capital city of assyria")
  (say "Antioch"))

(category ("what is the airspeed velocity * unladen swallow")
  (say "African or European?"))

(category ("what *")
  (say "I don't know what."))

(category ("is *")
  (randomly
   (say "Heck, yeah!")
   (say "Not a chance.")
   (say "Maybe, maybe not.")
   (say "I haven't really thought about it.")))

(category ("are *" rest)
   (reduce "is *" rest))

(category ("will *" rest)
  (reduce "is *" rest))

(category ("has *" rest)
  (reduce "is *" rest))

(category ("did *" rest)
  (reduce "is *" rest))

(category ("can *" rest)
  (reduce "is *" rest))

(category ("correct")
  (randomly
   (say "Woot!")
   (say "Do I get a prize?")
   (say "I'm on a roll today.")
   (say "I can't help but be good.")))

(category ("right")
  (reduce "correct"))

(category ("are you *" adjective)
  (randomly
   (say "Some say I'm *" adjective)
   (say "I was freakin born *" adjective)
   (say "who says I'm *?" adjective)))

(category ("thank you")
  (randomly
   (say "You're welcome")
   (say "No problem")
   (say "Hey, I enjoy this sort of thing")
   (say "The pleasure was all mine")
   (say "No worries, mate")
   (say "Oh, it was nothing")
   (say "Let me know if there's anything else")
   (say "Don't mention it")
   (say "Anytime")
   (say "Likewise")))

(category ("thanks")
  (reduce "thank you"))

(category ("you there")
  (reduce "are you there"))

(category ("are you there")
  (randomly
    (say "Right here!")
    (say "Yep")
    (say "Affirmative")
    (say "Aye aye, cap'n")))

(category ("go away")
  (randomly
    (say "Fine, then.")
    (say "Have it your way."))
  (do
   (let ((message *last-message*))
     (if (char= #\# (char (first (arguments message)) 0))
         (irc:part *connection* (first (arguments message)))
         (pushnew (source message) *ignored-nicks* :test #'string-equal)))))
(category ("piss off")
  (reduce "go away"))

(category ("you should *" report)
  (do
   (with-open-file (ouf (orca-path "data/unanswered.txt")
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
     (write-line report ouf)))
  (say "Ok, I've filed it as a bug report."))

(category ("would you *" report)
  (reduce "you should *" report))

(category ("*" anything)
  (do
   (with-open-file (ouf (orca-path "data/unanswered.txt")
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
     (write-line anything ouf)))
  (randomly
   (say (substitute #\space #\newline
                    (with-output-to-string (str)
                      (sb-ext:run-program "fortune"
                                          '("zippy")
                                          :search t
                                          :input nil
                                          :output str))))))
