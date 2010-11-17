(sentence -> to-be-productive person needs-to full-verb-phrase ".")
(the-goals -> (or "our mission"
                  "company values"
                  "increasing shareholder value"
                  "long-term profitability"
                  "brand viability"
                  "the departmental welfare"))
(to-be-productive -> (or "" "" "" "" "" "" "" "" "" ""
                         "at the end of the day,"
                         "we are highly incentivized for"
                         "over the next several weeks,"
                         "to increase shareholder value,"
                         "to push the envelope,"
                         "to get on the same page,"
                         "to bring our \"A\" game,"
                         "focusing on solutions,"
                         "with all due respect,"
                         "honestly,"
                         ("bringing" noun-phrase "to the table,")
                         "if we think outside the box,"
                         ("it's critical to" the-goals "that")))
(needs-to -> (or "is advised to"
                 "is encouraged to"
                 "needs to"
                 "must"
                 "can"
                 "will"
                 "should be able to"
                 "should"))
(full-verb-phrase -> verb-phrase prep-phrase)
(full-verb-phrase -> verb-phrase "and then" verb-phrase)
(full-verb-phrase -> "both" verb-phrase "and" verb-phrase)
(full-verb-phrase -> "either" verb-phrase "or" verb-phrase)
(full-verb-phrase -> "merge" noun-phrase "into" noun-phrase)
(full-verb-phrase -> verb-phrase "while we" full-verb-phrase)
(full-verb-phrase -> verb-phrase "until we" full-verb-phrase)
(noun-adjective -> (? adv) adj)
(noun-adjective -> adj "," adj)
(adjective-phrase ->
                  (? adv)
                  (or adj adj adj adj adj adj adj adj adj adj adj adj
                      (adj conj adj)
                      (adj "," adj ", or" adj)
                      (adj "," adj ", and" adj)))
(multi-adjective-phrase -> (or (adj conj adj)
                               (adj "," adj ", or" adj)
                               (adj "," adj ", and" adj)))
(full-noun -> noun-adjective n)
(plural-noun -> noun-adjective pn)
(noun-phrase -> (or "the" "a" "some") full-noun)
(noun-phrase -> (or "some" "many" "all" "all our" "all of our") plural-noun)
(noun-phrase -> noun-phrase prep-phrase)
(verb-phrase -> "actualize" noun-phrase)
(verb-phrase -> "advise on" noun-phrase)
(verb-phrase -> "allocate the bandwidth to" noun-phrase)
(verb-phrase -> "audit" noun-phrase "for" noun-phrase)
(verb-phrase -> "be" multi-adjective-phrase)
(verb-phrase -> "bring" noun-phrase "to the table")
(verb-phrase -> "build" noun-phrase)
(verb-phrase -> "champion" noun-phrase)
(verb-phrase -> "circle back around")
(verb-phrase -> "circle" noun-phrase "with" noun-phrase)
(verb-phrase -> "close the loop on" noun-phrase)
(verb-phrase -> "collaborate")
(verb-phrase -> "commoditize" noun-phrase)
(verb-phrase -> "communicate")
(verb-phrase -> "communicate" noun-phrase)
(verb-phrase -> "componentize" noun-phrase "into" noun-phrase)
(verb-phrase -> "connect" (? "ear-to-ear") "on" noun-phrase)
(verb-phrase -> "deep-dive")
(verb-phrase -> "descope" noun-phrase)
(verb-phrase -> "dialogue")
(verb-phrase -> "disambiguate" noun-phrase "from" noun-phrase)
(verb-phrase -> "disincent" noun-phrase)
(verb-phrase -> "drill-down into" noun-phrase)
(verb-phrase -> "drink the kool-aid")
(verb-phrase -> "eat our own dog food")
(verb-phrase -> "elevate to a" multi-adjective-phrase "level")
(verb-phrase -> "employ" noun-phrase)
(verb-phrase -> "enable" noun-phrase)
(verb-phrase -> "engage" noun-phrase)
(verb-phrase -> "ensure" noun-phrase)
(verb-phrase -> "envision" noun-phrase)
(verb-phrase -> "escalate" noun-phrase)
(verb-phrase -> "execute" noun-phrase)
(verb-phrase -> "facilitate" noun-phrase "in order to" verb-phrase)
(verb-phrase -> "feed" noun-phrase "through the" adj "pipeline")
(verb-phrase -> "focus")
(verb-phrase -> "gain traction with" noun-phrase)
(verb-phrase -> "get on the same page")
(verb-phrase -> "get our ducks in a row")
(verb-phrase -> "get approval on" noun-phrase)
(verb-phrase -> "get" noun-phrase "in our radar")
(verb-phrase -> "go forward")
(verb-phrase -> "go live on" noun-phrase)
(verb-phrase -> "go the extra mile to" verb-phrase)
(verb-phrase -> "harness" noun-phrase)
(verb-phrase -> "have a" multi-adjective-phrase "approach")
(verb-phrase -> "impact" noun-phrase)
(verb-phrase -> "implement" noun-phrase)
(verb-phrase -> "incentivize" noun-phrase)
(verb-phrase -> "instantiate" noun-phrase)
(verb-phrase -> "keep in the loop")
(verb-phrase -> "leapfrog" noun-phrase)
(verb-phrase -> "leverage" noun-phrase)
(verb-phrase -> "maximize" noun-phrase)
(verb-phrase -> "modularize" noun-phrase)
(verb-phrase -> "monetize" noun-phrase)
(verb-phrase -> "operationalize" noun-phrase)
(verb-phrase -> "peel the onion")
(verb-phrase -> "ping" noun-phrase)
(verb-phrase -> "pre-prepare to" verb-phrase)
(verb-phrase -> "process" noun-phrase)
(verb-phrase -> "productize" noun-phrase)
(verb-phrase -> "provide color")
(verb-phrase -> "push back")
(verb-phrase -> "push forward")
(verb-phrase -> "push the envelope")
(verb-phrase -> "raise the bar on" noun-phrase)
(verb-phrase -> "ramp up")
(verb-phrase -> "re-engineer" noun-phrase)
(verb-phrase -> "restructure" noun-phrase)
(verb-phrase -> "reach out to" noun-phrase)
(verb-phrase -> "repurpose" noun-phrase)
(verb-phrase -> "roll out" noun-phrase)
(verb-phrase -> "run it up the flagpole")
(verb-phrase -> "scope" noun-phrase)
(verb-phrase -> "show our commitment")
(verb-phrase -> "smartsize" noun-phrase)
(verb-phrase -> "spearhead" noun-phrase)
(verb-phrase -> "strategise" noun-phrase)
(verb-phrase -> "surface")
(verb-phrase -> "synergize" noun-phrase)
(verb-phrase -> "take ownership of" noun-phrase)
(verb-phrase -> "take the lead on" noun-phrase)
(verb-phrase -> "take" noun-phrase "to the next level")
(verb-phrase -> "task" noun-phrase)
(verb-phrase -> "think big")
(verb-phrase -> "throw" noun-phrase "under the bus")
(verb-phrase -> "touch base")
(verb-phrase -> "transfer" noun-phrase)
(verb-phrase -> "walk the talk")
(verb-phrase -> "whiteboard")
(adj -> (or "24/7"
            "2.0"
            "110% "
            "360 degree"
            "actionable"
            "aggressive"
            "ambitious"
            "back-to-back"
            "best of breed"
            "big picture"
            "cash-neutral"
            "central"
            "client-centered"
            "commoditized"
            "contextual"
            "core"
            "cradle-to-grave"
            "cross-functional"
            "cutting-edge"
            "directionally correct"
            "distributed"
            "empowering"
            "end-to-end"
            "enterprise"
            "functionally complete"
            "go-live"
            "goal-setting"
            "granular"
            "high-level"
            "historical"
            "impactful"
            "maximal"
            "mission-critical"
            "motivating"
            "multidisciplinary"
            "no-brainer"
            "offline"
            "out of pocket"
            "outside the box"
            "prioritized"
            "proactive"
            "quality"
            "real-time"
            "resource constrained"
            "robust"
            "rock star"
            "scalable"
            "seamless"
            "stand-alone"
            "state of the art"
            "strategic"
            "strong"
            "supervisory"
            "tacit"
            "team building"
            "user-centric"
            "waterfall"
            "win-win"
            "world-class"))
(adv -> (or "aggressively"
            "ambitiously"
            "confidently"
            "contextually"
            "functionally"
            "externally"
            "impactfully"
            "internally"
            "maximally"
            "notably"
            "proactively"
            "profitably"
            "programmatically"
            "properly"
            "scalably"
            "seamlessly"))
(conj -> (or "and" "but" "or"))
(n -> (or "review process"
          ("leading provider of" noun-phrase)
          "ROI"
          "architecture"
          "aspect"
          "bandwidth"
          "ballpark figures"
          "best of breed"
          "big picture"
          "business model"
          "business"
          "buy-in"
          "centers of excellence"
          "change catalyst"
          "chart porn"
          "closure"
          "close-of-play"
          "compatibility"
          "context"
          "contribution"
          "consensus"
          "critical path"
          "differentiator"
          "downsize"
          "driver"
          "ecosystem"
          "elevator story"
          "enabler"
          ("ETA on" noun-phrase)
          "event horizon"
          "facetime"
          "feature creep"
          "functionality"
          "game plan"
          "granularity"
          "heads-up"
          "helicopter view"
          "impact"
          "leverage"
          "learning experience"
          "long tail"
          "long-pole item"
          "low hanging fruit"
          "milestone"
          "mindset"
          "mindshare"
          "momentum"
          "measurable outcomes"
          "infrastructure"
          "new dimension"
          "next level"
          "organic growth"
          "organization"
          "paradigm shift"
          "paradigm"
          "performance management"
          "portfolio"
          "positioning"
          "planning"
          "pushback"
          "quick win"
          "ramp-up"
          "resource"
          "revenue"
          "roll-out"
          "rough order of magnitude"
          "scenario"
          "scope creep"
          "sea change"
          "secret sauce"
          "shareholder value"
          "skip-level"
          "sniff test"
          "situation"
          "S.M.A.R.T. goals"
          "solution"
          "solutioning"
          "space"
          "special sauce"
          "strategic gap"
          "straw man"
          "synergy"
          "takeaway"
          "task force"
          "tip of the iceberg"
          "traction"
          "transparency"
          "turnkey solution"
          "value chain"
          "value proposition"
          "value-add"
          "vertical"
          ("visibility of" noun-phrase)
          "wet signature"
          "wetware"
          "pony defense"
          "800 pound gorilla"
          "workflow"))
(pn -> (or "key players"
           "best practices"
           "buckets"
           "business"
           "challenges"
           "company values"
           "core competencies"
           "cycles"
           "domains"
           "deliverables"
           "dynamics"
           "initiatives"
           "learnings"
           "network effects"
           "next steps"
           "obligations"
           "opportunities"
           "growth opportunities"
           "planning activities"
           "reports"
           "synergies"
           "team players"))
(prep-phrase -> (or "aboard" "about" "above" "across" "after" "against" "along"
                    "amid" "among" "around" "as" "at" "before" "behind" "below"
                    "beneath" "beside" "besides" "between" "beyond" "by"
                    "concerning" "considering" "despite" "during" "except"
                    "excepting" "excluding" "following" "for" "from" "in" "inside"
                    "into" "like" "minus" "near" "of" "off" "on" "onto" "opposite"
                    "outside" "over" "past" "per" "plus" "regarding"
                    "save" "since" "than" "through" "to" "toward" "towards"
                    "under" "underneath" "unlike" "upon" "versus" "via"
                    "with" "within" "without") noun-phrase)
(prep-phrase -> "in a timely manner")
(prep-phrase -> "from the get-go")
(prep-phrase -> "in this space")