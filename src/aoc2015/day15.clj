(ns aoc2015.day15
  (:require [clojure.string :as str]))

"Day 15: Science for Hungry People
 
 exactly 100 teaspoons of ingredients
 list of the remaining ingredients you could use
 properties per teaspoon
    capacity (how well it helps the cookie absorb milk)
    durability (how well it keeps the cookie intact when full of milk)
    flavor (how tasty it makes the cookie)
    texture (how it improves the feel of the cookie)
    calories (how many calories it adds to the cookie)
 total score: 
    adding up each of the properties (negative totals become 0)
    multiplying together everything except calories
"

(comment
  "Counting possibilities"

  (defn f [order n]
    (cond
      (zero? order) 1
      (= 1 order) n
      :else (reduce + (map #(f (dec order) %) (range 1 (inc n))))))

  (time (map #(vector (inc %) (f % 100)) (range 0 6)))
  ;; => ([1 1] [2 100] [3 5050] [4 171700] [5 4421275] [6 91962520])
  ;;   
  "So a seq of every 4-tuples where the elements sum to 100 has 172k entries 
   (not these aren't exact - it's actually 176851, see below. Not sure why, don't care)")

(defn two-tuples-adding-to [n]
  (map (fn [i] (list i (- n i))) (range 0 (inc n))))

(defn tuple-summing-to [n length]
  (if (= 2 length) (two-tuples-adding-to n)
      (mapcat
       #(map (fn [tt] (cons % tt)) (tuple-summing-to (- n %) (dec length)))
       (range 0 (inc n)))))

(def measures (tuple-summing-to 100 4))
(count measures)
;; => 176851

(def example [[-1 -2 6 3 8] [2 3 -2 -1 3]])

(defn scalar-mult [scalar vect]
  (map #(* scalar %) vect))

(defn score [measures ingredients]
  (apply * (butlast (map #(max 0 %) (apply map + (map scalar-mult measures ingredients))))))

(comment
  (score [44 56] example)
  ;; => 62842880


  (apply max (map #(score % example) (tuple-summing-to 100 2)))
  ;; => 62842880
  )

(def ingredients [[5 -1 0 0 5]
                  [-1 3 0 0 1]
                  [0 -1 4 0 6]
                  [-1 0 0 2 8]])
(comment
  (time (apply max (map #(score % ingredients) (tuple-summing-to 100 4))))
  ;; "Elapsed time: 2565.204427 msecs"
  ;; 13882464
  )

(defn score2 [measures ingredients]
  (let [xs (map #(max 0 %) (apply map + (map scalar-mult measures ingredients)))]
    (if (not= 500 (last xs)) 0
        (apply * (butlast xs)))))

(comment
  (apply max (map #(score2 % example) (tuple-summing-to 100 2)))
  ;; => 57600000

  (time (apply max (map #(score2 % ingredients) (tuple-summing-to 100 4))))
  ;; "Elapsed time: 2485.345468 msecs"
  ;; 11171160
  )