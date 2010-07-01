(sentence -> setup)

(twist -> (or "" "" "" "" "" "" "" "" "" "" ""
              "" "" "" "" "" "" "" "" "" "" ""
              "before time runs out"
              ("before" a-character "arrives")
              ("before" a-group "arrives")
              ("while being chased by" a-group)
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

(basic-setup -> the-main-characters "are fighting" a-group)
(basic-setup -> the-main-characters "are seeking" a-place)
(basic-setup -> the-main-characters "are attempting to find" a-character)
(basic-setup -> the-main-characters "must protect" a-macguffin "from" a-group)
(basic-setup -> the-main-characters "are stranded in" a-place "and must escape")
(basic-setup -> the-main-characters "must win" (a game) "against" a-group)
(basic-setup -> the-main-characters "is imprisoned in" a-place)

(basic-setup -> the-main-character "is defending" a-place "from" a-group)
(basic-setup -> the-main-character "is infiltrating" a-group)
(basic-setup -> the-main-character "has fallen in love with" a-character)
(basic-setup -> the-main-character "attempts to destroy" a-group)
(basic-setup -> the-main-character "attempts to escape" a-group)
(basic-setup -> the-main-character "is stealing" a-macguffin "from" a-group)
(basic-setup -> the-main-character "voyages to" a-place "to" do-something)
(basic-setup -> the-main-character "is seeking" a-place)
(basic-setup -> the-main-character "is chasing" a-group)
(basic-setup -> the-main-character "is being blackmailed by" a-character)
(basic-setup -> the-main-character "is stranded")
(basic-setup -> the-main-character "must clean up" a-place)
(basic-setup -> the-main-character "must solve the murder of" a-character)
(basic-setup -> the-main-character "must solve the theft of" a-macguffin)
(basic-setup -> the-main-character "must escort" a-character "to" a-place)
(basic-setup -> the-main-character "must escort" a-group "to" a-place)
(basic-setup -> the-main-character "helps" a-group "flee to" a-place)
(basic-setup -> the-main-character "helps" a-group "deal with" a-group)
(basic-setup -> the-main-character "stumble into" a-place "and must escape from" a-group)
(basic-setup -> the-main-character "is suddenly attacked by" a-character "and must learn why")
(basic-setup -> the-main-character "is spying on" a-group)
(basic-setup -> the-main-characters "must guard" a-place "against" a-group)
(basic-setup -> the-main-character "must prevent" a-group "from" doing-something)

(a-place -> (a
             (or "magical"
                 "far away"
                 "hidden"
                 "run-down"
                 "inaccessible"
                 "dangerous"
                 "doomed"
                 "enemy")
             (or "land"
                 "base"
                 "home"
                 "shelter"
                 "fortress"
                 "tower"
                 "pit")))

(macguffin-adjective -> (or "magical"
                            "priceless"
                            "fake"
                            "technological"
                            "prized"))


(macguffin -> (or "ring"
                  "gem"
                  "device"
                  "artifact"
                  "sword"
                  "bomb"
                  "crystal"
                  "hostage"
                  "spaceship"
                  "briefcase"
                  "document"
                  "paper"
                  "necklace"
                  "scroll"
                  "statue"
                  "treasure"))

(a-macguffin -> (or ((a macguffin-adjective macguffin))
                    ("the" macguffin-adjective macguffin)
                    ("the one" macguffin-adjective macguffin)
                    ("three" macguffin-adjective (plural macguffin))
                    ("five" macguffin-adjective (plural macguffin))
                    ("seven" macguffin-adjective (plural macguffin))))

(doing-something -> (or ("fighting against" a-group)
                        ("searching for" a-macguffin)
                        ("destroying" a-macguffin)
                        ("delivering" a-macguffin)
                        ("healing" a-character)
                        ("joining" a-group)))

(do-something -> (or ("fight against" a-group)
                     ("search for" a-macguffin)
                     ("destroy" a-macguffin)
                     ("deliver" a-macguffin)
                     ("heal" a-character)
                     ("join" a-group)))

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
                            "queer"
                            "retired"
                            "scrappy"
                            "sexy"
                            "short"
                            "solitary"
                            "stern"
                            "tall"
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
                        "enterprising"))
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
(the-main-character -> a-character)
(the-main-characters -> a-group)
(the-main-characters -> the-main-character "and" a-group)