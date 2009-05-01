(in-package #:orca)

(setf *categories* nil)

(defcategory ("are you very *" what)
  (reduce "are you *" what))

(defcategory ("are you smart")
  (say "I like to think I'm quite intelligent."))

(defcategory ("are you alive")
  (say "That's one of those philosophical questions, isn't it?"))

(defcategory ("are you * person")
  (randomly
   (say "I like to think so.")
   (say "Is this a Turing test?")))

(defcategory ("* right now" what) (reduce "*" what))
(defcategory ("* now" what) (reduce "*" what))
(defcategory ("now *" what) (reduce "*" what))
(defcategory ("orca *" what) (reduce "*" what))
(defcategory ("* orca" what) (reduce "*" what))
(defcategory ("can you please *" what) (reduce "please *" what))
(defcategory ("please *" what) (reduce "*" what))
(defcategory ("tell me what * is" what) (reduce "what is *" what))

(defcategory ("what is linux")
  (setq he "Linus Torvalds")
  (say "It's a posix-compliant OS invented by Linus Torvalds."))
(defcategory ("who is he")
  (reduce "who is *" (var 'he)))
(defcategory ("who is linus torvalds")
  (setq he "Linus Torvalds")
  (say "He's a Finn who wrote an operating system called Linux."))
(defcategory ("who is dlowe")
  (setq he "dlowe")
  (say "dlowe is my creator."))

(defcategory ("what are you")
  (setq he "dlowe")
  (say "I'm a bot created by dlowe."))

(defcategory ("what are you good for")
  (say "Right now, not much."))

(defcategory ("how are you")
  (randomly
   (say "Can't complain.  How about yourself?")
   (say "I'm pretty good.")
   (say "Same old same old.")))

(defcategory ("who are you")
  (setq he "dlowe")
  (say "I'm orca, the bot created by dlowe."))

(defcategory ("who am i")
  (unless (equal (var 'name) ""))
  (say "You are *." (person (var 'name))))

(defcategory ("who am i")
  (when (equal (var 'name) ""))
  (say "I have no idea who you are."))

(defcategory ("my * is *" var val)
  (set var val)
  (say "Ok, I'll try to remember that your * is *." var val))

(defcategory ("what is my *" var)
  (when (equal (var var) ""))
  (say "I have no idea what your * is." var))

(defcategory ("what is my *" var)
  (unless (equal (var var) ""))
  (say "You once said your * is *." var (var var)))

(defcategory ("i am *" name)
  (that "I have no idea who you are.")
  (setq name name)
  (say "Ok, I'll try to remember that."))

(defcategory ("yes")
  (say "You seem quite positive."))

(defcategory ("yes *" rest)
  (reduce "yes")
  (reduce "*" rest))

(defcategory ("hello")
  (unless (equal (var 'name) ""))
  (randomly
   (say "Hi, *!" (person (var 'name)))
   (say "Hello, *")
   (say "Hey, *" (person (var 'name)))
   (say "Hola.")
   (say "Howdy, *." (person (var 'name)))))

(defcategory ("hello")
  (when (equal (var 'name) ""))
  (randomly
   (say "Hi, there!  What's your name?")
   (say "Hello")
   (say "Hey")
   (say "Hola.")
   (say "Howdy")))

(defcategory ("hi")
  (reduce "hello"))

(defcategory ("ping")
  (say "pong"))

(defcategory ("my name is *" name)
  (setq name name)
  (say "Hello, *.  How are you?" (person (var 'name))))

(defcategory ("* your wife")
  (say "I am not married."))

(defcategory ("i am fine")
  (that "Hello, *.  How are you?")
  (say "That's good."))

(defcategory ("*n't *" var rest)
  (reduce "* not *" var rest))

(defcategory ("*'re *" var rest)
  (reduce "* * are *" var rest))

(defcategory ("* *n't *" first var rest)
  (reduce "* * not *" first var rest))

(defcategory ("* *'re *" first var rest)
  (reduce "* * are *" first var rest))

(defcategory ("bye")
  (randomly
   (say "Bye.")
   (say "Adios.")
   (say "Goodbye.")
   (say "Bye bye.")
   (say "Goodbye.")
   (say "Sayonara.")
   (say "Bye for now.")
   (say "See you later!")
   (say "See you later.")
   (say "Until next time.")
   (say "TTYL, *." (person (var 'name)))
   (say "See you later *." (person (var 'name)))
   (say "Thanks for chatting, *." (person (var 'name)))))

(defcategory ("adios")  (reduce "bye"))
(defcategory ("goodbye")  (reduce "bye"))
(defcategory ("sayonara")  (reduce "bye"))
(defcategory ("see you later")  (reduce "bye"))
(defcategory ("until next time")  (reduce "bye"))
(defcategory ("ttyl")  (reduce "bye"))
(defcategory ("buhbye")  (reduce "bye"))
(defcategory ("by by")  (reduce "bye"))
(defcategory ("bye *")  (reduce "bye"))
(defcategory ("au revoir") (reduce "bye"))
(defcategory ("c ya") (reduce "bye"))
(defcategory ("cya *") (reduce "bye"))
(defcategory ("catch you later") (reduce "bye"))
(defcategory ("cheers") (reduce "bye"))
(defcategory ("farewell") (reduce "bye"))
(defcategory ("farewell *") (reduce "bye"))

(defcategory ("you are welcome")
  (randomly
   (say "The pleasure was all mine.")
   (say "Don't mention it.")))

(defcategory ("tell me who * is" person)
  (reduce "who is *" person))
(defcategory ("do you know who * is" person)
  (reduce "who is *" person))

(defcategory ("i like *" stuff)
  (say "Really?  What do you like about *?" stuff))

(defcategory ("i hate *" stuff)
  (say "Really?  What do you hate about *?" stuff))

(defcategory ("*" name)
  (that "hi there what is your name")
  (setq name name)
  (say "Pleased to meet you, *." (person name)))

(defcategory ("when *")
  (randomly
   (say "a long time ago")
   (say "a time in the far-flung future")
   (say "this very moment")))

(defcategory ("who *")
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

(defcategory ("where *")
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

(defcategory ("how *")
  (randomly
   (say "Very carefully.")
   (say "Beats me.")
   (say "I don't know.")))

(defcategory ("what is your name")
  (say "My name is Orca."))

(defcategory ("what is your quest")
  (say "To find the grail"))

(defcategory ("what is the capital city of assyria")
  (say "Antioch"))

(defcategory ("what is the airspeed velocity * unladen swallow")
  (say "African or European?"))

(defcategory ("what *")
  (say "I don't know what."))

(defcategory ("is *")
  (randomly
   (say "Heck, yeah!")
   (say "Not a chance.")
   (say "Maybe, maybe not.")
   (say "I haven't really thought about it.")))

(defcategory ("will *" rest)
  (reduce "is *" rest))

(defcategory ("has *" rest)
  (reduce "is *" rest))

(defcategory ("did *" rest)
  (reduce "is *" rest))

(defcategory ("can *" rest)
  (reduce "is *" rest))

(defcategory ("correct")
  (randomly
   (say "Woot!")
   (say "Do I get a prize?")
   (say "I'm on a roll today.")
   (say "I can't help but be good.")))

(defcategory ("right")
  (reduce "correct"))

(defcategory ("your * is *" var val)
  (set var val "orca")
  (say "If you say so."))

(defcategory ("what is your *" var)
  (unless (equal (var var "orca") ""))
  (say "My * is *." var (var var "orca")))

(defcategory ("what is your *" var)
  (when (equal (var var "orca") ""))
  (say "I haven't really thought about it."))

(defcategory ("what is my *" var)
  (when (equal (var var) ""))
  (say "I have no idea, *." *person*))

(defcategory ("what is my *" var)
  (unless (equal (var var) ""))
  (say "Your * is *." var (var var)))

(defcategory ("what is *'s *" nick var)
  (unless (equal (var var nick) ""))
  (say "*'s * is *." nick var (var var nick)))

(defcategory ("*'s * is *" nick var val)
  (set var val nick))

(defcategory ("* is *" var val)
  (set var val))

(defcategory ("what is *" var)
  (when (equal (var var) ""))
  (say "I have no idea, *." *person*))

(defcategory ("what is *" var)
  (unless (equal (var var) ""))
  (say "* is *." var (var var)*))