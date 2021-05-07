(ns aoc2015.day21
  (:require [clojure.test :refer [deftest is]]))

"player (you) and the enemy (the boss) take turns attacking. 
 The player always goes first.
 You have 100 hit points.
 The first character at or below 0 hit points loses.
 Damage dealt is equal to damage score defender's armor score
 attacker always does at least 1 damage
 increased by buying items in exchange for gold
 You must buy exactly one weapon; 
 Armor is optional, but you can't use more than one. 
 You can buy 0-2 rings

 5 weapon options, 6 armor options (including 0). 7 ring options (including none)

 How many total combinations?
 5*6*7*7=1470
 "

(def weapons [[8 4 0]
              [10 5 0]
              [25 6 0]
              [40 7 0]
              [74 8 0]])

(def armor [[0 0 0]
            [13 0 1]
            [31 0 2]
            [53 0 3]
            [75 0 4]
            [102 0 5]])

(def rings #{[0 0 0]
             [25 1 0]
             [50 2 0]
             [100 3 0]
             [20 0 1]
             [40 0 2]
             [80 0 3]})

(def combos (sort-by first (for [w weapons
                                 a armor
                                 r1 rings
                                 r2 (conj (disj rings r1) [0 0 0])]
                             (map + w a r1 r2))))
(count combos)

(def boss [104 8 1])

(defn win? [hp [_cost p-attack p-armor] [boss-hp b-attack b-armor]]
  (let [p-damage (max 1 (- p-attack b-armor))
        b-damage (max 1 (- b-attack p-armor))]
    (>= (Math/ceil (/ hp b-damage)) (Math/ceil (/ boss-hp p-damage)))))

"after 1 turn: hp = hp-damage
 after n turns hp = hp - n damage
 looking for hp-ndamge<=0
 hp<=n damage
 hp/damage <= n"

(first (drop-while #(not (win? 100 % boss)) combos))
;; => (78 8 1)

(first (drop-while #(win? 100 % boss) (reverse combos)))
;; => (148 7 2)

(deftest t
  (is (win? 8 [nil 5 5] [12 7 2]))
  (is (win? 100 [nil 8 1] [104 8 1]))
  (is (not (win? 100 [nil 7 2] [104 8 1]))))
