(thing-adj
 "advanced"
 "artificial"
 "atomic"
 "beta"
 "biomechanical"
 "bionic"
 "binary"
 "cybernetic"
 "clockwork"
 "crystalline"
 "digital"
 "dual"
 "electronic"
 "forbidden"
 "functional"
 "global"
 "hydraulic"
 "intelligent"
 "inhuman"
 "journeying"
 "kinetic"
 "knowledgable"
 "lifelike"
 "mechanical"
 "networked"
 "obedient"
 "positronic"
 "quantum"
 "robotic"
 "synthetic"
 "transforming"
 "upgraded"
 "vigilant"
 "versatile"
 "wireless"
 "xperimental"
 "xtraterrestrial"
 "ytterbium"
 "zeta")

(activity-adj
 "accurate"
 "basic"
 "ceaseless"
 "dangerous"
 "efficient"
 "forbidden"
 "galactic"
 "hazardous"
 "immediate"
 "journeying"
 "justified"
 "jeopardous"
 "kamikaze"
 "lethal"
 "logical"
 "mechanical"
 "mandatory"
 "infinite"
 "intensive"
 "interstellar"
 "nocturnal"
 "online"
 "potential"
 "rational"
 "scientific"
 "solar"
 "space"
 "terran"
 "ultimate"
 "worldwide"
 "widespread"
 "xperimental"
 "xpert"
 "yearly"
 "yucky"
 "zealous")

(activity
 "analysis"
 "assassination"
 "acquisition"
 "battle"
 "bioscience"
 "burning"
 "calculation"
 "capture"
 "chaos"
 "destruction"
 "exploration"
 "fighting"
 "fun"
 "geophysics"
 "gratification"
 "gunfighting"
 "harm"
 "infiltration"
 "investigation"
 "incineration"
 "judo"
 "jobs"
 "jealousy"
 "killing"
 "kindness"
 "learning"
 "mathematics"
 "nullification"
 "obliteration"
 "observation"
 "peacekeeping"
 "patrolling"
 "questioning"
 "quantification"
 "repair"
 "sabotage"
 "troubleshooting"
 "utility"
 "violence"
 "vengeance"
 "warfare"
 "wrecking"
 "wayfaring"
 "xenocide"
 "xecution"
 "yelling"
 "yardwork"
 "zoology"
 "zymurgy"
)


(for
 "assembled for"
 "built for"
 "calibrated for"
 "designed for"
 "engineered for"
 "fabricated for"
 "generated for"
 "hardwired for"
 "intended for"
 "justified for"
 "keen on"
 "limited to"
 "manufactured for"
 "normally for"
 "optimized for"
 "programmed for"
 "qualified for"
 "responsible for"
 "skilled in"
 "trained for"
 "used for"
 "viable for"
 "wanting"
 "xperienced in"
 "yearning for"
 "zoned for")

(thing
 "android"
 "being"
 "construct"
 "device"
 "entity"
 "facsimile"
 "guardian"
 "humanoid"
 "individual"
 "judge"
 "juggernaut"
 "knight"
 "lifeform"
 "machine"
 "neohuman"
 "organism"
 "person"
 "quadruplet"
 "replicant"
 "soldier"
 "technician"
 "unit"
 "varient"
 "worker"
 "xenomorph"
 "youth"
 "zombie")

;; 1
(phrase thing)

;; 2
(phrase thing-adj thing)

;; 3
(phrase activity-adj activity thing)
(phrase activity "and" activity thing)

;; 4
(phrase activity "and" activity-adj activity thing)
(phrase activity-adj activity-adj activity thing)
(phrase activity-adj activity "and" activity thing)
(phrase thing-adj thing for activity)
(phrase thing for activity "and" activity)
(phrase thing-adj activity-adj activity thing)
(phrase thing for activity-adj activity)

;; 5
(phrase thing-adj thing-adj thing for activity)
(phrase thing-adj thing for activity "and" activity)
(phrase thing-adj thing for activity-adj activity)
(phrase thing for activity "and" activity-adj activity)

;; 6
(phrase activity-adj activity-adj activity "and" activity-adj activity thing)
(phrase thing for activity "," activity-adj activity "and" activity-adj activity)
(phrase thing-adj thing for activity "and" activity-adj activity)
(phrase thing-adj thing-adj activity "and" activity-adj activity thing)
(phrase thing-adj thing-adj thing for activity "and" activity-adj activity)
(phrase thing-adj thing-adj thing for activity-adj activity "and" activity-adj activity)

;; 7
(phrase thing-adj thing-adj thing for activity-adj activity "and" activity)
(phrase thing-adj thing-adj thing for activity "and" activity-adj activity)

;; 8
(phrase thing-adj thing-adj thing for activity-adj activity "and" activity-adj activity)

;; 9
(phrase thing-adj thing-adj thing for activity-adj activity "and" thing-adj activity-adj activity)

;; 10
(phrase thing-adj thing-adj thing for activity-adj activity "," activity-adj activity "and" activity-adj activity)

;; 11
(phrase thing for activity "and" activity-adj activity "/" thing-adj thing for activity "and" activity-adj activity)
(phrase thing-adj thing for activity "and" activity "/" thing-adj thing for activity-adj activity "and" activity)
(phrase thing-adj thing for activity "and" activity "/" thing-adj thing for activity "and" activity-adj activity)
