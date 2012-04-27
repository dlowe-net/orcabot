(sentence -> throws (a food-item) at target ".  " result)
(eats -> (or "eats" "consumes" "gobbles" "absorbs"
             "bolts" "devours" "feeds upon" "inhales" "puts away"
             "polishes off" "wolfs down on"))
(adverb -> (or "angrily" "apologetically" "arrogantly"
               "bitterly" "boldly" "brutally" "callously" "calmly" "carefully"
               "cautiously" "cheerfully" "contentedly" "craftily" "crazily"
               "creepily" "curiously" "defiantly" "desperately"
               "diabolically" "dismally" "disdainfully" "drunkenly"
               "dreamily" "enthusiastically" "excitedly" "fearfully"
               "fiercely" "fondly" "gently" "graciously" "gruffly"
               "grumpily" "happily" "hungrily" "impatiently"
               "inquisitively" "lazily" "loudly" "lustily"
               "passionately" "politely" "quickly" "quietly"
               "rudely" "ruthlessly" "savagely" "seductively" "sensually"
               "shamelessly" "solemnly" "stoically" "thoughtfully"
               "valiantly"))
(sentence-self -> adverb eats (a food-item))
(throws -> (or "throws" "tosses" "lobs" "casts" "chucks" "flings" "heaves" "hurls" "lets fly" "slings"))
(at -> (or "at" "towards"))
(result -> (or "It hits!" "It hits!" "It hits!" "It hits!"
               "It misses!" "It misses!" "It misses!" "It misses!" "It misses!"
               ("It misses and hits" bystander "!")))
(food-prepared -> (or "chocolate-covered"
                      "homemade"
                      "mass-produced"
                      "braised"
                      "boiled"
                      "fried"
                      "deep fried"
                      "roasted"
                      "gourmet"
                      "grilled"
                      "raw"
                      "Hungarian"
                      "Irish"))
(meat -> (or "chicken"
             "turkey"
             "pork"
             "beef"
             "duck"
             "veal"
             "venison"
             "goat"
             "tuna"
             "salmon"))
(fruit -> (or "apple"
              "banana"
              "pear"
              "peach"
              "plum"
              "mango"))
(vegetable -> (or "carrot"
                  "celery"
                  "lettuce"
                  "arugula"
                  "onion"))
(grain -> (or ("wheat"
               "oat"
               "rice")))
(protein -> (or "egg"
                "black bean"
                "pinto bean"))
(cheese -> (or "cheddar"
               "parmesan"
               "gouda"
               "provolone"
               "brie"
               "swiss"
               "mozzarella"
               "muenster"
               "colby"
               "goat cheese"))
(nut -> (or "cashew"
            "walnut"
            "peanut"))
(flavor -> (or "chocolate"
               "vanilla"
               "strawberry"
               "blueberry"
               "coffee"))
(ingredient -> (or meat
                   fruit
                   vegetable
                   cheese
                   protein))
(ingredients -> (or ingredient
                    (ingredient "and" ingredient)
                    (ingredient "," ingredient ", and" ingredient)))
(salad-greens -> (or "spinach"
                     "lettuce"
                     "iceburg lettuce"
                     "spring mix"
                     "arugula"))
(salad-dressing -> (or (fruit "vinegarette")
                       "balsamic vinegarette"
                       "oil and vinegar"))
(food-item -> (? food-prepared)
           (or ingredient
               (ingredients "stew")
               (ingredients "pasta")
               (ingredients "fricasee")
               (ingredients "curry")
               (ingredients "chili")
               (ingredients "sandwich")
               (ingredients "soup")
               (ingredient "on a stick")
               ("filet of" meat)
               (meat "pie")
               (fruit "pie")
               (fruit nut "bread")
               (fruit nut "salad")
               (fruit cheese "salad")
               (salad-greens "with" salad-dressing)
               (flavor "cupcake")
               (flavor "candy")
               "mashed potatoes"
               "corn on the cob"
               "meatloaf"
               "brownie"
               "hamburger"
               "twinkie"
               "corn dog"))