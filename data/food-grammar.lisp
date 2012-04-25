(sentence -> nick throws (a food-item) at target ".  " result)
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
(protein -> (or "eggs"
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
               (flavor "cupcake")
               (flavor "candy")
               "brownie"
               "hamburger"
               "twinkie"
               "corn dog"))