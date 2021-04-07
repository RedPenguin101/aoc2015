(ns aoc2015.day13
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

"Day 13: Knights of the Dinner Table
 
 going to find the optimal seating arrangement
 circular table
 8 people
 exactly two neighbors
 
 Alice would lose 2 happiness units by sitting next to David.
 
 8! permutations again, ~ 40k. Probably can brute force it
 represent the order as a seq, a nested map for the happiness units
 
 API:
 net-happiness :: happiness order
 person :: order idx
 neighbours :: order idx"

(def example "Alice would gain 54 happiness units by sitting next to Bob.\nAlice would lose 79 happiness units by sitting next to Carol.\nAlice would lose 2 happiness units by sitting next to David.\nBob would gain 83 happiness units by sitting next to Alice.\nBob would lose 7 happiness units by sitting next to Carol.\nBob would lose 63 happiness units by sitting next to David.\nCarol would lose 62 happiness units by sitting next to Alice.\nCarol would gain 60 happiness units by sitting next to Bob.\nCarol would gain 55 happiness units by sitting next to David.\nDavid would gain 46 happiness units by sitting next to Alice.\nDavid would lose 7 happiness units by sitting next to Bob.\nDavid would gain 41 happiness units by sitting next to Carol.")

(defn parse [acc string]
  (let [[_ from gl h to] (re-find #"(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+)." string)]
    (assoc-in acc [from to] (* (if (= gl "gain") 1 -1) (Long/parseLong h)))))

(def x (reduce parse {} (str/split-lines example)))

(defn neighbours [order idx]
  (let [ppl (count order)
        left (mod (dec idx) ppl)
        right (mod (inc idx) ppl)]
    [(nth order idx) (nth order left) (nth order right)]))

(neighbours (keys x) 0)

(defn happiness [lookup [person left right]]
  (+ (get-in lookup [person left])
     (get-in lookup [person right])))

(defn solve [lookup order]
  (reduce (fn [acc idx]
            (+ acc (happiness lookup (neighbours order idx))))
          0
          (range 0 (count order))))

(solve x ["David" "Alice" "Bob" "Carol"])

(solve x (last (sort-by #(solve x %) (combo/permutations (keys x)))))

(comment
  (time (let [inp (reduce parse {} (str/split-lines (slurp "resources/day13input")))]
          (solve inp (last (sort-by #(solve inp %) (combo/permutations (keys inp)))))))
  ;; => 733 (in 12 seconds)

  "12 seconds for ~40k tests, about 0.3 ms per test, probably not going to speed that up 
   meaningfully without being smarter. Adding a 9th person would be 400k, probably 2mins or so
   Not good enough."

  (/ 12000 40000)

  (time (let [input (reduce parse {} (str/split-lines (slurp "resources/day13input")))
              others (keys input)
              new-input (-> (into {} (map (fn [[k v]] [k (assoc v "Me" 0)]) input))
                            (assoc "Me" (zipmap others (repeat 0))))]
          (solve new-input (last (sort-by #(solve new-input %) (combo/permutations (keys new-input)))))))
  ;; => 725 (in 122secs)
  )