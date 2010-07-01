(sentence -> setup)

(setup -> the-main-characters "are fighting" a-group ".")
(setup -> the-main-characters "are seeking" a-place ".")
(setup -> the-main-characters "are attempting to find" a-character ".")
(setup -> the-main-characters "must protect" a-macguffin "from" a-group ".")
(setup -> the-main-characters "are stranded in" a-place ".")

(setup -> the-main-character "is defending" a-place "from" a-group ".")
(setup -> the-main-character "is infiltrating" a-group ".")
(setup -> the-main-character "has fallen in love with" a-character ".")
(setup -> the-main-character "attempts to destroy" a-group ".")
(setup -> the-main-character "attempts to escape" a-group ".")
(setup -> the-main-character "is stealing" a-macguffin "from" a-group ".")
(setup -> the-main-character "voyages to" a-place "to" do-something ".")
(setup -> the-main-character "is seeking" a-place ".")
(setup -> the-main-character "is chasing" a-group ".")
(setup -> the-main-character "is being blackmailed by" a-character ".")
(setup -> the-main-character "is stranded in" a-place ".")

(a-place -> (a
             (or "magical"
                 "far away"
                 "hidden"
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
                            "technogical"
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
                    "true love"
                    ("the" macguffin-adjective macguffin)
                    ("three" macguffin-adjective (plural macguffin))
                    ("five" macguffin-adjective (plural macguffin))
                    ("seven" macguffin-adjective (plural macguffin))))

(do-something -> (or ("fight against" a-group)
                     ("search for" a-macguffin)
                     ("destroy" a-macguffin)
                     ("deliver" a-macguffin)
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
                            "cool"
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