(problem -> "global warming")
(sentence -> diagnosis fix)
(bad-adjective -> (or "offline"
                      "behaving abnormally"
                      "unstable"
                      "failing"
                      "disrupted"
                      "collapsing"))
(bad-thing -> (or
               "a fluctuation"
               "an error"
               "a failure"
               "a power surge"
               "an instability"
               "a disruption"
               "an anomaly"
               "an interference"
               "a malfunction"))
(bad-things -> (or
               "fluctuations"
               "power surges"
               "errors"
               "variences"
               "anomalies"
               "flaws"
               "logical inconsistencies"
               "failures"
               "malfunctions"))
(diagnosis ->
           (or
            ("we have detected that" bad-things "are causing" problem)
            ("scanners indicate" bad-things "may be the source of" problem)
            (problem "is caused by" bad-things "in" qualified-noun-phrase)
            (problem "is being caused by" bad-thing "in" qualified-noun-phrase)
            (bad-things "in" qualified-noun-phrase "are causing" problem)
            (bad-thing "in" qualified-noun-phrase "is causing" problem)
            ("we have" problem "because" subject-to-be bad-adjective))
           (or "." "!"))
(fix -> directive solution ".")
(fix -> directive solution "by" method ".")
(fix -> method "should" solution ".")
(directive -> "we" (or "need to"
                       "must"
                       "can"
                       "will"
                       "could"
                       "might be able to"
                       "should probably"))
(solution -> (or
              ("align the" part "with" qualified-noun-phrase)
              ("calibrate the" part "against" qualified-noun-phrase)
              ("overload the" part "with" qualified-noun-phrase)
              ("realign the" part "with" qualified-noun-phrase)
              ("adjust the" part "with" qualified-noun-phrase)
              ("force the" part "to" solution)
              ("equalize the" part "and the" part)
              ("destabilize the" part "with" qualified-noun-phrase)
              ("neutralize the" part "with" qualified-noun-phrase)
              ("trim the" part "with" qualified-noun-phrase)
              ("regulate the" part "with" qualified-noun-phrase)
              ("stabilize the" part "with" qualified-noun-phrase)
              ("reverse the" part "with" qualified-noun-phrase)))
(method -> (or
            ("aligning" qualified-noun-phrase)
            ("calibrating" qualified-noun-phrase)
            ("overloading" qualified-noun-phrase)
            ("realigning" qualified-noun-phrase)
            ("adjusting" qualified-noun-phrase)
            ("forcing" qualified-noun-phrase)
            ("equalizing" qualified-noun-phrase)
            ("destabilizing" qualified-noun-phrase)
            ("neutralizing" qualified-noun-phrase)
            ("trimming" qualified-noun-phrase)
            ("regulating" qualified-noun-phrase)
            ("stabilizing" qualified-noun-phrase)
            ("reversing" qualified-noun-phrase)))
(part -> tech-kind (or n pn))
(subject-to-be -> (or
                   ("a" (? tech-kind) n "is")
                   ("the" (? tech-kind) n "is")
                   ("the" (? tech-kind) pn "are")
                   ("some" (? tech-kind) pn "are")
                   ("our extra" (? tech-kind) pn "are")))
(qualified-noun-phrase -> (or
                           ("a" (? tech-kind) n)
                           ("the" (? tech-kind) n)
                           ("the" (? tech-kind) pn)
                           ("some of our" (? tech-kind) pn)
                           ("extra" (? tech-kind) pn)))
(tech-kind -> (or "primary"
                  "secondary"
                  "main"
                  "forward"
                  "aft"
                  "front"
                  "rear"
                  "gravitational"
                  "fusion"
                  "positronic"
                  "dynamic"
                  "baryonic"
                  "static"
                  "ionic"))
(material -> (or
              "dilithum"
              "aluminium"))
(subatomic -> (or "tachyon"
                  "neutrino"
                  "proton"
                  "photon"
                  "antilepton"
                  "positron"
                  "electron"
                  "baryonic"
                  "gamma"
                  "magnetic"
                  "subspace"))
(tech -> (or "subspace"
             "warp"
             "antimatter"
             "impulse"
             "quantum"
             "nano-"
             "polarity"
             "metagenic"
             "ionic"))
(n -> (or
       "battle bridge"
       "holodeck"
       (tech "reactor")
       (tech "emitter")
       "tractor beam"
       (tech "core")
       (tech "pad")
       (tech "coil")
       (tech "deflector")
       "pattern buffer"
       (tech "pattern")
       (tech "array")
       (tech "pump")
       "replicator"
       (tech "polarity")
       (tech "bubble")
       (tech "transciever")
       (subatomic "flow")
       (subatomic "flux")
       (subatomic "field")
       (subatomic "polarity")
       (subatomic "emitter")))
(pn -> (or
        "algorithms"
        "circuits"
        "sail towers"
        (tech "chambers")
        "disruptors"
        (tech "conduits")
        (material "crystals")))


(prep-phrase -> (or
                 ("around the" noun-phrase)
                 ("with a" noun-phrase)))