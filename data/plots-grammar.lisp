(sentence -> setup)

(twist -> (or "" "" "" "" "" "" "" "" "" "" ""
              "" "" "" "" "" "" "" "" "" "" ""
              "before time runs out"
              "without being discovered"
              ("before" a-character "arrives")
              ("before" a-group "arrives")
              ("before" some-people "arrive")
              ("while being chased by" those-people)
              ("but first must" do-something)
              ("while" doing-something)
              ("and must" do-something)))

(setup -> basic-setup twist ".")

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

(basic-setup -> the-main-characters "are fighting" those-people)
(basic-setup -> the-main-characters "are seeking" a-place)
(basic-setup -> the-main-characters "have been imprisoned by" a-character)
(basic-setup -> the-main-characters "are attempting to find" a-character)
(basic-setup -> the-main-characters "must protect" a-thing "from" those-people)
(basic-setup -> the-main-characters "are stranded in" a-place "and must escape")
(basic-setup -> the-main-characters "must win" (a game) "against" those-people)
(basic-setup -> the-main-characters "are imprisoned in" a-place "by" those-people)
(basic-setup -> the-main-characters "must protect" a-place "from" those-people)
(basic-setup -> the-main-characters "must impersonate" those-people "to" do-something)
(basic-setup -> the-main-characters "voyage to" a-place "to" do-something)

(basic-setup -> a-group "is attempting to assassinate" the-main-character)
(basic-setup -> some-people "are attempting to assassinate" the-main-character)
(basic-setup -> the-main-character "is imprisoned in" a-place "by" those-people)
(basic-setup -> the-main-character "is defending" a-place "from" those-people)
(basic-setup -> the-main-character "is infiltrating" a-group)
(basic-setup -> the-main-character "has fallen in love with" a-character)
(basic-setup -> the-main-character "attempts to destroy" those-people)
(basic-setup -> the-main-character "attempts to escape" those-people)
(basic-setup -> the-main-character "is stealing" a-thing "from" those-people)
(basic-setup -> the-main-character "voyages to" a-place "to" do-something)
(basic-setup -> the-main-character "is seeking" a-place)
(basic-setup -> the-main-character "is chasing" those-people)
(basic-setup -> the-main-character "is being blackmailed by" a-character)
(basic-setup -> the-main-character "is stranded")
(basic-setup -> the-main-character "must clean up" a-place)
(basic-setup -> the-main-character "must train" those-people "to" do-something)
(basic-setup -> the-main-character "must solve the murder of" a-character)
(basic-setup -> the-main-character "must solve the theft of" a-thing)
(basic-setup -> the-main-character "must escort" a-character "to" a-place)
(basic-setup -> the-main-character "must escort" those-people "to" a-place)
(basic-setup -> the-main-character "must replace" a-thing "with" (a thing) "in" a-place)
(basic-setup -> the-main-character "helps" those-people "flee to" a-place)
(basic-setup -> the-main-character "helps" those-people "deal with" those-people)
(basic-setup -> the-main-character "stumbles into" a-place "and must escape from" those-people)
(basic-setup -> the-main-character "is suddenly attacked by" a-character "and must learn why")
(basic-setup -> the-main-character "is spying on" those-people)
(basic-setup -> the-main-characters "must guard" a-place "against" those-people)
(basic-setup -> the-main-character "must prevent" those-people "from" doing-something)
(basic-setup -> the-main-character "must steal" a-thing "for" those-people)
(basic-setup -> the-main-character "must steal" a-thing "for" a-character)
(basic-setup -> the-main-character "must steal" a-thing "to" do-something)
(basic-setup -> the-main-character "is helping" a-character "to" do-something)
(basic-setup -> the-main-character "is fooled by" those-people "into" doing-something)
(basic-setup -> the-main-character "is trying to buy" a-thing "from" those-people)
(basic-setup -> the-main-character "is trying to sell" a-thing "to" those-people)

(a-place -> (a
             (or "magical"
                 "far away"
                 "hidden"
                 "run-down"
                 "inaccessible"
                 "dangerous"
                 "doomed"
                 "high-class"
                 "expensive"
                 "cheap"
                 "poor"
                 "quaint"
                 "enemy")
             (or "land"
                 "beach"
                 "mountain"
                 "valley"
                 "river"
                 "base"
                 "hotel"
                 "home"
                 "art gallery"
                 "shelter"
                 "village"
                 "fortress"
                 "tower"
                 "pit")))

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
                        ("joining" those-people)))

(do-something -> (or ("fight against" those-people)
                     ("search for" a-thing)
                     ("destroy" a-thing)
                     ("deliver" a-thing)
                     ("heal" a-character)
                     ("join" those-people)))

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
                        "wizard"))
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
                        character-adjective))
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
              "league"
              "order"
              "organization"
              "society"
              "sorority"
              "syndicate"
              "tribe"))

(a-group ->
         (a group-adjective group)
         "of"
         character-adjective
         (plural basic-character))
(some-people -> character-adjective (plural basic-character))
(those-people -> some-people)
(those-people -> a-group)
(the-main-character -> a-character)
(the-main-characters -> a-group)
(the-main-characters -> the-main-character "and" a-group)
(the-main-characters -> the-main-character "and" some-people)