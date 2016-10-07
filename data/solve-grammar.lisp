(problem -> (or "global warming"
                "system abends"
                "disk head crashes"
                "shark attacks"
                "hurricanes"
                "boil water alerts"
                "the Riders of Nazgul"
                "frogs falling from the sky"
                "artillery bombardment"
                "build failures"
                "performance regressions"
                "failed deployments"
                "Oracle Service Requests"
                "severe packet loss"
                "departmental miscommunications"))
(sentence -> diagnosis fix)
(bad-adjective -> (or "offline"
                      "behaving abnormally"
                      "giving strange readings"
                      "unstable"
                      "failing"
                      "disrupted"
                      "collapsing"
                      "not responding"))
(bad-thing -> (or
               "a fluctuation"
               "an error"
               "a failure"
               "a power surge"
               "an instability"
               "a disruption"
               "an anomaly"
               "a flaw"
               "an interference"
               "a malfunction"))
(bad-things -> (or
               "fluctuations"
               "power surges"
               "errors"
               "flaws"
               "stress fractures"
               "variances"
               "anomalies"
               "disturbances"
               "flaws"
               "logical inconsistencies"
               "failures"
               "malfunctions"))
(problem-sources -> bad-things "in" qualified-noun-phrase)
(are-causing -> (or "are causing"
                   "could cause"
                   "may be the source of"))
(is-caused-by -> (or "is caused by"
                     "is being caused by"
                     "could be caused by"
                     "might be caused by"
                     "could originate in"))
(diagnosis ->
           (or
            ("we have detected that" problem-sources are-causing problem)
            (tech "sensors indicate" problem-sources are-causing problem)
            ("our" scanner "indicates that" problem-sources are-causing problem)
            (problem is-caused-by bad-things "in" qualified-noun-phrase)
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
(directive -> "we'll have to")
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
              ("reverse the" part "with" qualified-noun-phrase)
              ("emit" emission "from" qualified-noun-phrase)))
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
            ("reversing" qualified-noun-phrase)
            ("firing" projectile "at" qualified-noun-phrase)
            ("emitting" emission "from" qualified-noun-phrase)))

(part -> tech-kind (or n pn))
(subject-to-be -> (or
                   ("a" n "is")
                   ("the" n "is")
                   ("the" (? tech-kind) pn "are")
                   ("some" (? tech-kind) pn "are")
                   ("our extra" (? tech-kind) pn "are")))
(qualified-noun-phrase -> (or
                           ("a" n)
                           ("the" n)
                           ("the" (? tech-kind) pn)
                           ("some of our" (? tech-kind) pn)))
(class-designation -> "class" (or "A" "B" "D"))
(level-designation -> "level" (or "5" "9"))
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
                  "ionic"
                  "massive"
                  "unstable"
                  class-designation
                  level-designation))
(material -> (or
              "dilithum"
              "aluminium"
              "adamantium"
              "beryllium"
              "carbon"
              "lead"
              "thallium"
              "gold"))
(subatomic -> (or "tachyon"
                  "neutrino"
                  "proton"
                  "photon"
                  "antilepton"
                  "positron"
                  "matter"
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
(system -> (or "weapons"
               "navigation"
               "life-support"
               "guidance"
               "docking"
               "warp"
               (tech-kind "control"))
        (or "computer"
            "system"))
(scanner -> (? (or "long-range"
                   "short-range"))
         subatomic
         (or "scanner"
             "locator"
             "detector"
             "sensor"))
(n -> "space-time continuum")
(n -> scanner)
(n -> (? tech-kind)
   (or
    "battle bridge"
    "holodeck"
    "pattern buffer"
    "recognition protocols"
    "replicator"
    "tractor beam"
    system
    (subatomic "emitter")
    (subatomic "field")
    (subatomic "flow")
    (subatomic "flux")
    (subatomic "polarity")
    (subatomic "stream")
    (subatomic "cannons")
    (tech "array")
    (tech "bubble")
    (tech "coil")
    (tech "compensator")
    (tech "core")
    (tech "deflector")
    (tech "emitter")
    (tech "integrator")
    (tech "locator")
    (tech "pad")
    (tech "pattern")
    (tech "polarity")
    (tech "pump")
    (tech "reactor")
    (tech "scanner")
    (tech "singularity")
    (tech "resonator")
    (tech "transciever")))
(pn -> (or
        "algorithms"
        "circuits"
        "sail towers"
        "disruptors"
        (tech "inducers")
        (tech "sensors")
        (tech "chambers")
        (tech "nullifiers")
        (tech "translators")
        (tech "transponders")
        (tech "conduits")
        (subatomic "waves")
        (subatomic "pulses")
        (material "crystals")))

(projectile -> (a (? tech-kind)
                  (or
                   (subatomic "pulse")
                   (subatomic "field")
                   (subatomic "wave"))))

(emission -> (? tech-kind)
          (or
           (material "particles")
           (subatomic "waves")
           (subatomic "pulses")))

(prep-phrase -> (or
                 ("around the" noun-phrase)
                 ("with a" noun-phrase)))
