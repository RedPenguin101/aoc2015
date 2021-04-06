(ns aoc2015.day9
  (:require [clojure.string :as str]
            [clojure.test :refere [deftest are is]]
            [clojure.math.combinatorics :as combo]))



(comment
  "Day 9: All in a Single Night
 
   Locations
   distances between every pair of locations
   must visit each location exactly once
   What is the shortest distance
 
   Shortest path visiting every node of a weighted graph. Sounds hard to solve with anything except brute force! But brute
   force might be OK. There are 28 pairs of inputs, and 8 nodes, so every node must be connected to every other."

  (defn connections [i] (if (= 2 i) 1 (+ (dec i) (connections (dec i)))))
  (connections 8)
  ;; => 28

  "So if we generate every possible permutation of the 8 nodes, then calculate the distance of that path, we should be good
   
   how many permutations? I think 8 factorial:"

  (defn factorial [n] (if (= n 1) 1 (* n (factorial (dec n)))))
  (factorial 8)
  ;; => 40320

  "About 40k, that should be fine
   
   So I can grab the nodes from the input, that's easy enough
   
   I need an API where I can query the from and to and get the distance. A hashmap with redundant entries should be fine:"

  "Faerun to Tristram = 65"

  {"Faerun" {"Tristram" 65}
   "Tristram" {"Faerun" 65}}

  "Then just a get-in will do the trick. The nodes are then just the keys of this, which is a bonus
   
   So for an API
   
   HI:  shortest-route :: graph -> route
   MED: route-length :: graph, route -> distance
   LO: "
  1)

(defn parse [string]
  (let [[_ from to dist] (re-find #"(\w+) to (\w+) = (\d+)" string)]
    [from to (Long/parseLong dist)]))

(def input
  (reduce (fn [acc [from to dist]]
            (-> acc
                (assoc-in [from to] dist)
                (assoc-in [to from] dist)))
          {}
          (map parse (str/split-lines (slurp "resources/day9input")))))

(def example
  (reduce (fn [acc [from to dist]]
            (-> acc
                (assoc-in [from to] dist)
                (assoc-in [to from] dist)))
          {}
          (map parse (str/split-lines "London to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141"))))

(defn route-length [graph route]
  (reduce + (map #(get-in graph %) (partition 2 1 route))))

(defn shortest-route [graph]
  (let [nodes (keys graph)]
    (route-length graph (first (sort-by #(route-length graph %) (combo/permutations nodes))))))

(defn longest-route [graph]
  (let [nodes (keys graph)]
    (route-length graph (last (sort-by #(route-length graph %) (combo/permutations nodes))))))

(comment
  (route-length example ["Dublin" "London" "Belfast"])
  (shortest-route example)
  (time (shortest-route input))
  ;; => 117 (in about 5.5 seconds)

  (longest-route example)
  (time (longest-route input))
  ;; => 909 (in about 5.5 seconds)

  (float (/ 5500 (factorial 8)))
  ;; => 0.13640873

  "Turned out very slow. Calculating the route distance is about 0.13ms per permuatation. Which seems fair.
   
   To get this under 1 second for execution then, I'd need to do at most 7.5k tests"

  (/ 1000 0.13)
  ;; => 7692.307692307692

  (first (sort-by #(route-length input %) (combo/permutations (keys input))))
  ;; => ("Faerun" "AlphaCentauri" "Tambi" "Snowdin" "Norrath" "Tristram" "Arbre" "Straylight")
  (route-length input '("Faerun" "AlphaCentauri" "Tambi" "Snowdin" "Norrath" "Tristram" "Arbre" "Straylight"))
  ;; => 117

  "The way to eliminate tests is like: if you calculate a->b->c, and it's longer than your shortest complete path, then you don't have 
   to test anything that starts with a->b->c. This seems very possible: for example, the distance from Faerun to Straylight directly is
   137: longer than the total shortest path. So provided you know that you know that shortest complete path, that would eliminate the
   need to test anything that is faerun->straylight->,,,"

  "But I'm too lazy to implement :(
   
   Also it wouldn't work for the longest solution.")

