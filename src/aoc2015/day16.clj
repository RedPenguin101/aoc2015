(ns aoc2015.day16
  (:require [clojure.string :as str]))



(comment
  "Day 16: Aunt Sue
   
   You have 500 Aunts named Sue.
   number 1 to 500
   MFCSAM can detect quantity of distince compounds in a sample
   
    children
    cats
    dog: 
      samoyeds, 
      pomeranians, 
      akitas, 
      vizslas.
    goldfish
    trees
    cars
    perfumes
   
  Results:
    children: 3
    cats: 7
    samoyeds: 2
    pomeranians: 3
    akitas: 0
    vizslas: 0
    goldfish: 5
    trees: 3
    cars: 2
    perfumes: 1
      
  Things missing from your list aren't zero - you simply don't remember the value.
  So parse the input and see which one is a submap of the target 
")

(defn map-vals [f m] (into {} (map (fn [[k v]] [k (f v)]) m)))
(defn parse-long [x] (Long/parseLong x))
(defn sub-map? [m1 m2] (= m2 (select-keys m1 (keys m2))))

(defn parse [string]
  (let [[sue-number & attrs] (str/split (subs string 4) #"[:,] ")]
    {sue-number (map-vals parse-long (apply hash-map attrs))}))

(def input (apply merge (map parse (str/split-lines (slurp "resources/day16input")))))

(def target {"children" 3
             "cats" 7
             "samoyeds" 2
             "pomeranians" 3
             "akitas" 0
             "vizslas" 0
             "goldfish" 5
             "trees" 3
             "cars" 2
             "perfumes" 1})

(some (fn [[k v]] (when (sub-map? target v) k)) input)
;; => "213"

(comment
  "the output from the machine isn't exact values - some of them indicate ranges
   cats and trees readings indicates that there are greater than that many
   while the pomeranians and goldfish readings indicate that there are fewer than that many")

(def target2 {"children" 3
              "samoyeds" 2
              "akitas" 0
              "vizslas" 0
              "cars" 2
              "perfumes" 1})

(defn sub-map-with-rules? [value]
  (and (sub-map? target2 (apply dissoc value ["cats" "trees" "pomeranians" "goldfish"]))
       (> (or (get value "cats") ##Inf) 7)
       (> (or (get value "trees") ##Inf) 3)
       (< (or (get value "pomeranians") 0) 3)
       (< (or (get value "goldfish") 0) 5)))

(some (fn [[k v]] (when (sub-map-with-rules? v) k)) input)
;; => "323"
