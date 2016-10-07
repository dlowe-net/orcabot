(sentence -> (or solo-plot group-plot))

(possible-complication -> (or "" "" "" "" "" "" "" "" "" "" ""
                              "" "" "" "" "" "" "" "" "" "" ""
                              "before time runs out"
                              "without being discovered"
                              ("before" a-character "arrives")
                              ("before" a-group "arrives")
                              ("before" some-people "arrive")
                              ("while being chased by" those-people)
                              ("and is forced to" do-something)
                              ("but first must" do-something)
                              ("while" doing-something)
                              ("and must" do-something)))

(game -> (or "football match"
             "poker match"
             "wrestling match"
             "fencing match"
             "race"
             "blackjack game"
             "baseball game"
             "basketball game"
             "dodgeball game"
             "quiddich match"))

(main-group-plot -> "They" do-something possible-complication)
(main-group-plot -> "They" do-something "and" do-something)
(main-solo-plot -> (or "he" "she") does-something possible-complication)
(main-solo-plot -> (or "he" "she") does-something "and" does-something)
(main-solo-plot -> "They" do-something possible-complication)
(main-solo-plot -> "They" do-something "and" do-something)

(possible-group-twist -> (or "" "" "" "" "" "" "" "" group-twist))

(group-twist ->
             (or "" "" "" "" "" "" "" ""
                 "to everyone's surprise,"
                 "incredibly,"
                 "suddenly,")
             (or "one of them turns traitor"
                 ("one of them falls in love with" a-character)
                 ("they find they've been working for " a-group "all along"))
             ".")

(possible-solo-twist -> (or "" "" "" "" "" "" "" "" solo-twist))
(solo-twist ->
             (or "to everyone's surprise,"
                 "incredibly,"
                 "suddenly,"
                 "without warning,")
             (or ((or "he" "she") "betrays" a-group)
                 ((or "he" "she") "falls in love with" a-character)
                 ((or "he" "she") "has been working for " a-group "all along")
                 ((or ("she finds out that" a-character "is actually her")
                      ("he finds out that" a-character "is actually his"))
                  (or "father" "mother" "son" "daughter" "uncle" "aunt" "old roommate" "old teacher")))
             ".")

(climax ->
        (or ("Once" (or "he" "she") does-something ",")
            ("Once they" do-something ",")
            ("When" (or "he" "she") does-something ",")
            ("When they" do-something ","))
        (or ("there is a climactic battle between" a-group "and" a-group)
               (a-character "and" a-character "get into a fist-fight")
               ("the secret of" a-place "is unearthed")
               ("there's a dramatic standoff between" a-character "and" a-character)
               "the Elder Gods are awakened")
        ".")

(finally -> (or "In the end,"
                "Finally,"
                "Afterwards,"
                "As a result,"
                "Eventually,"))

(a-group-finale-happens -> (or "the entire world explodes"
                               "they all live happily ever after"
                               "everything returns to normal"
                               "they all turn out to be dead"
                               "everyone dies horribly"
                               "they all marry each other"
                               "they go back home"
                               "nobody wins"
                               "they ride off into the sunset"))

(a-solo-finale-happens -> (or "the entire world explodes"
                              "they all live happily ever after"
                              "everything returns to normal"
                              "everyone dies horribly"
                              "nobody wins"
                              (a-character "dies and everyone is sad")
                              (a-character "recovers from illness")
                              (a-group (or "wins" "loses") (a game))
                              ((or "he" "she") "returns home")
                              ((or "he" "she") "gets married and settles down")
                              ((or "he" "she") "rides off into the sunset")))

(group-plot -> group-setup possible-complication "." main-group-plot ".  " possible-group-twist climax (? finally a-group-finale-happens "."))

(group-setup -> the-main-characters "are fighting" those-people)
(group-setup -> the-main-characters "are seeking" a-place)
(group-setup -> the-main-characters "have been imprisoned by" a-character)
(group-setup -> the-main-characters "are attempting to find" a-character)
(group-setup -> the-main-characters "must protect" a-thing "from" those-people)
(group-setup -> the-main-characters "are stranded in" a-place)
(group-setup -> the-main-characters "are trapped in" a-place)
(group-setup -> the-main-characters "must win" (a game) "against" those-people)
(group-setup -> the-main-characters "are imprisoned in" a-place "by" those-people)
(group-setup -> the-main-characters "must protect" a-place "from" those-people)
(group-setup -> the-main-characters "must impersonate" those-people "to" do-something)
(group-setup -> the-main-characters "voyage to" a-place "to" do-something)

(solo-plot -> solo-setup possible-complication "." main-solo-plot ".  " possible-solo-twist climax (? finally a-solo-finale-happens "."))

(solo-setup -> a-group "is attempting to assassinate" the-main-character)
(solo-setup -> some-people "are attempting to assassinate" the-main-character)
(solo-setup -> the-main-character "is imprisoned in" a-place "by" those-people)
(solo-setup -> the-main-character "is defending" a-place "from" those-people)
(solo-setup -> the-main-character "is infiltrating" a-group)
(solo-setup -> the-main-character "has fallen in love with" a-character)
(solo-setup -> the-main-character "attempts to destroy" those-people)
(solo-setup -> the-main-character "attempts to escape" those-people)
(solo-setup -> the-main-character "is stealing" a-thing "from" those-people)
(solo-setup -> the-main-character "voyages to" a-place "to" do-something)
(solo-setup -> the-main-character "is seeking" a-place)
(solo-setup -> the-main-character "is chasing" those-people)
(solo-setup -> the-main-character "is being blackmailed by" a-character)
(solo-setup -> the-main-character "is stranded")
(solo-setup -> the-main-character "must clean up" a-place)
(solo-setup -> the-main-character "must train" those-people "to" do-something)
(solo-setup -> the-main-character "must solve the murder of" a-character)
(solo-setup -> the-main-character "must solve the theft of" a-thing)
(solo-setup -> the-main-character "must escort" a-character "to" a-place)
(solo-setup -> the-main-character "must escort" those-people "to" a-place)
(solo-setup -> the-main-character "must replace" a-thing "with" (a thing) "in" a-place)
(solo-setup -> the-main-character "helps" those-people "flee to" a-place)
(solo-setup -> the-main-character "helps" those-people "deal with" those-people)
(solo-setup -> the-main-character "stumbles into" a-place "and must escape from" those-people)
(solo-setup -> the-main-character "is suddenly attacked by" a-character "and must learn why")
(solo-setup -> the-main-character "is spying on" those-people)
(solo-setup -> the-main-characters "must guard" a-place "against" those-people)
(solo-setup -> the-main-character "must prevent" those-people "from" doing-something)
(solo-setup -> the-main-character "must steal" a-thing "for" those-people)
(solo-setup -> the-main-character "must steal" a-thing "for" a-character)
(solo-setup -> the-main-character "must steal" a-thing "to" do-something)
(solo-setup -> the-main-character "is helping" a-character "to" do-something)
(solo-setup -> the-main-character "is fooled by" those-people "into" doing-something)
(solo-setup -> the-main-character "is trying to buy" a-thing "from" those-people)
(solo-setup -> the-main-character "is trying to sell" a-thing "to" those-people)
(solo-setup -> the-main-character "seduces" a-character)
(solo-setup -> the-main-character "'s soul is trapped in" a-thing)
(solo-setup -> a-character "seduces" the-main-character)

(place-adj -> (or "magical"
                 "far away"
                 "hidden"
                 "run-down"
                 "inaccessible"
                 "dangerous"
                 "doomed"
                 "high-class"
                 "abandoned"
                 "expensive"
                 "cheap"
                 "poor"
                 "prestigious"
                 "quaint"
                 "enemy"))
(a-place -> (a (? place-adj)
               (or "land"
                   "beach"
                   "mountain"
                   "valley"
                   "river"
                   "base"
                   "hotel"
                   "home"
                   "garden"
                   "jungle"
                   "prison"
                   "laboratory"
                   "tomb"
                   "school"
                   "labyrinth"
                   "art gallery"
                   (thing "factory")
                   "shelter"
                   "village"
                   "fortress"
                   "tower"
                   "pocket dimension"
                   "pool hall"
                   "library"
                   "office building"
                   "pit")))
(a-place -> (or "Boston" "Hell" "Heaven" "London"
                "Moscow" "Paris" "Rome" "Beijing"
                "Los Angeles" "Sydney" "Tokyo"
                "Antartica"))

(thing-adjective -> (or "magical"
                        "priceless"
                        "broken"
                        "nuclear"
                        "mysterious"
                        "alien"
                        "personal"
                        "technological"
                        "prized"))


(thing -> (or "ring"
              "gem"
              "device"
              "artifact"
              "sword"
              "bomb"
              "crystal"
              "generator"
              "hostage"
              "spaceship"
              "briefcase"
              "airplane"
              "zepplin"
              "document"
              "paper"
              "necklace"
              "radio"
              "scroll"
              "statue"
              "treasure"))

(a-thing -> (or ((a thing-adjective thing))
                    ("the" thing-adjective thing)
                    ("the one" thing-adjective thing)
                    ("three" thing-adjective (plural thing))
                    ("five" thing-adjective (plural thing))
                    ("seven" thing-adjective (plural thing))))

(doing-something -> (or ("fighting against" those-people)
                        ("searching for" a-thing)
                        ("destroying" a-thing)
                        ("delivering" a-thing)
                        ("healing" a-character)
                        ("joining" those-people)
                        ("assassinating" a-character)
                        ("imprisoning" a-character)
                        ("stealing" a-thing)
                        ("escorting" a-character "to" a-place)
                        ("spying on" a-character)
                        ("seducing" a-character)
                        ("winning" (a game))
                        ("losing" (a game))))

(do-something -> (or ("fight against" those-people)
                     ("search for" a-thing)
                     ("destroy" a-thing)
                     ("steal" a-thing)
                     ("deliver" a-thing)
                     ("heal" a-character)
                     ("join" those-people)
                     ("assassinate" a-character)
                     ("imprison" a-character)
                     ("escort" a-character "to" a-place)
                     ("spy on" a-character)
                     ("seduce" a-character)
                     ("win" (a game))
                     ("lose" (a game))))

(does-something -> (or ("fights against" those-people)
                     ("searches for" a-thing)
                     ("destroys" a-thing)
                     ("steals" a-thing)
                     ("delivers" a-thing)
                     ("heals" a-character)
                     ("joins" those-people)
                     ("assassinates" a-character)
                     ("imprisons" a-character)
                     ("escorts" a-character "to" a-place)
                     ("spies on" a-character)
                     ("seduces" a-character)
                     ("wins" (a game))
                     ("loses" (a game))))

(character-adjective -> (or "absent-minded"
                            "aggressive"
                            "alcoholic"
                            "asshole"
                            "empathic"
                            "ethnic"
                            "evil"
                            "inept"
                            "insane"
                            "insufferable"
                            "older"
                            "younger"
                            "badass"
                            "barbarian"
                            "beleaguered"
                            "benevolent"
                            "bookish"
                            "bungling"
                            "charming"
                            "clumsy"
                            "deceitful"
                            "deceased"
                            "faceless"
                            "forgetful"
                            "freewheeling"
                            "genius"
                            "gentle"
                            "good"
                            "hard-working"
                            "hideously ugly"
                            "loner"
                            "lovable"
                            "mechanical"
                            "nameless"
                            "noble"
                            "purple"
                            "queer"
                            "retired"
                            "scrappy"
                            "sexy"
                            "short"
                            "solitary"
                            "stern"
                            "tall"
                            "undead"
                            "wild"
                            "wise"))
(basic-character -> (or "archmage"
                        "actor"
                        "adventurer"
                        "secret agent"
                        "brat"
                        "bum"
                        "champion"
                        "clown"
                        "coward"
                        "crook"
                        "damsel"
                        "detective"
                        "dog"
                        "drug dealer"
                        "engineer"
                        "everyman"
                        "fairy"
                        "foreigner"
                        "gangster"
                        "gas station attendant"
                        "genie"
                        "ghost"
                        "healer"
                        "hippie"
                        "hustler"
                        "inventor"
                        "janitor"
                        "kid"
                        "librarian"
                        "loser"
                        "messenger"
                        "miser"
                        "misfit"
                        "musician"
                        "native american"
                        "nerd"
                        "occultist"
                        "performer"
                        "priest"
                        "prince"
                        "princess"
                        "professor"
                        "scholar"
                        "scientist"
                        "serial killer"
                        "sex kitten"
                        "soldier"
                        "spy"
                        "survivor"
                        "teacher"
                        "thief"
                        "victim"
                        "warrior"
                        "widow"
                        "witness"
                        "wizard"
                        "zombie"))
(a-basic-character -> (a character-adjective basic-character))
(a-character -> (or ((repeat 20 a-basic-character))
                    ("the" character-adjective
                           "daughter of"
                           a-basic-character)
                    ("the" character-adjective
                           "son of"
                           a-basic-character)
                    (a-basic-character "wanna-be")
                    (a-basic-character "-worshipper")))
(group-adjective -> (or "merciless"
                        "benevolent"
                        "mysterious"
                        "supernatural"
                        "ragtag"
                        "ancient"
                        "small"
                        "forgotten"
                        "enterprising"
                        "united"))
(group -> (or "alliance"
              "band"
              "bunch"
              "circle"
              "clan"
              "clique"
              "club"
              "coalition"
              "confederation"
              "crew"
              "crowd"
              "family"
              "federation"
              "fellowship"
              "fraternity"
              "gang"
              "guild"
              "horde"
              "league"
              "order"
              "organization"
              "society"
              "sorority"
              "syndicate"
              "tribe"))

(a-group -> (repeat 20
                    (a (or character-adjective group-adjective) group)
                    "of" character-adjective (plural basic-character)))
(a-group -> (repeat 10 "the" group-adjective group "of" (plural basic-character)))

(a-group -> "a horde of zombies")
(a-group -> "the Illuminati")
(a-group -> "the KGB")
(a-group -> "the Nazis")
(a-group -> "the CIA")
(a-group -> "ITA Software")
(some-people -> (repeat 4 character-adjective (plural basic-character)))
(some-people -> (plural basic-character) "from" a-group)
(those-people -> some-people)
(those-people -> a-group)
(the-main-character -> a-character)
(the-main-characters -> a-group)
(the-main-characters -> the-main-character "and" a-group)
(the-main-characters -> the-main-character "and" some-people)
