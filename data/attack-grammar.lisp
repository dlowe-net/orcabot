(sentence -> (or suggestion command))

(suggestion -> suggest-preamble attack "?")
(suggest-preamble -> (or "Might I suggest"
                         "Can I suggest"
                         "Should we attack immediately with"))

(command -> command-preamble attack "!")
(command-preamble -> (or ("We should attack" thing "now with")
                         ("We must attack" thing "now with")
                         ("I suggest attacking" thing "with")
                         ("We should coordinate an attack with")))

(attack -> method-of-attack type-of-attack "with" (? attack-adjective) attack-object "and" attack-object-two)

(method-of-attack -> (a (or "full-frontal"
                            "pincer" "surprise" "brutally excessive" "multi-pronged" "glorious"
                            "violent" "devastating" "superior" "fast-paced" "fleet-wide" "stealth"
                            "diversionary" "exceptional" "point-blank" "night time"
                            "acid-heavy" "immediate" "overwhelming" "unstoppable" "underground" "aerial"
                            "naval" "amphibious" "full-scale")))
(type-of-attack -> (or "assault" "attack" "bombardment" "offensive" "barrage" "charge" "strike" "operation"
                       "maneuver" "blitzkrieg" "ambush" "massacre"))

(attack-adjective -> (or "laser" "berserker" "acid" "armoured attack" "proton"
                         "three kinds of" "atomic" "toxic" "explosive"
                         "red-hot" "thermal" "automated fire" "cluster"
                         "enhanced germ" "energy-drink-fueled" "battle ready" "Sontaran" "military"))
(attack-object -> (or "bees" "chainsaws" "marmots" "acid" "monkeys" "mines" "bombs" "snakes" "spiders"
                      "knives" "rockets" "sharks" "owls" "repurposed cybermats" "cannons" "alligators" "ants"
                      "gorillas" "genetically enhanced cyber-elephants" "mechanoids" "KGB agents"
                      "MI5 operatives" "thermonuclear missiles"))
(attack-object-two -> (or "robots" "ninjas" "grenades" "a dolphin full of napalm" "dynamite"
                          "xenomorphs" "lots and lots of C4" "tactical nukes" "bio-weapons"
                          "rocket launchers" "an elephant" "a memory worm for afterwards" "this pencil"))
