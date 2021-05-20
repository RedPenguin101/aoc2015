(ns aoc2015.day24
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(def input [1 2 3 7 11 13 17 19 23 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113])

"Day 24: It Hangs in the Balance
 
 Seq of packages with weights
 3 compartments, each need to have the same weight
 1st compartment needs as few packages as possible
 Tiebreaker: 1st compartment with the smallest product of weights (Quantum Entanglement)
 
 What is the quantum entanglement of the first group of packages 
 in the ideal configuration?
 
 NOTES
 Weight in each compartment must be sum(p)/3"

(def example [1 2 3 4 5 7 8 9 10 11])
(apply + example)
;; => 60
(/ 60 3)
;; => 20

(/ (apply + input) 3)
;; => 520

(defn can-two-partition?
  ([s] (can-two-partition? '() '() (reverse (sort s))))
  ([s1 s2 s]
   (cond (empty? s) (when (= (apply + s1) (apply + s2)) [s1 s2])
         (> (apply + s1) (apply + s2)) (recur s1 (cons (first s) s2) (rest s))
         :else (recur (cons (first s) s1) s2 (rest s)))))

(can-two-partition? (reverse [1 2 3 4 5 7 8 10]))


"Thoughts on solution:
 This is the subset sum / partition problem, which is NP complete
 https://en.wikipedia.org/wiki/Partition_problem
 https://en.wikipedia.org/wiki/Subset_sum_problem
 
 First thought: exhaustive search"

(comment
  (combo/count-subsets input)
;; => 536_870_912

  (combo/count-subsets example)
;; => 1024

  "About half a billion subsets. Maybe doable to find every subset that sums
 to En/3?"

  (time (doall (filter #(= (apply + %) 20) (combo/subsets example))))
  ;; 11 ms

  (float (/ (* 536879912 (/ 1024 11)) 1000 60 60))
  ;; => 13883 hours - no good!

  (time (doall (filter #(= (apply + %) 520) (combo/subsets input))))

  (+ 1 1))


(combo/count-combinations input 6)
;; => 118755

(count (filter #(= (apply + %) 520) (combo/combinations input 6)))

(->> (combo/combinations input 6)
     (filter #(= (apply + %) 520))
     (sort-by #(apply * %))
     #_(map #(apply * %))
     (take 10)
     #_(some #(can-two-partition? (set/difference (set input) (set %)))))
'((1 89 101 107 109 113)
  (3 97 101 103 107 109)
  (7 83 101 107 109 113)
  (7 89 101 103 107 113)
  (11 79 101 107 109 113)
  (11 83 97 107 109 113)
  (11 83 101 103 109 113)
  (11 89 97 101 109 113)
  (11 89 97 103 107 113)
  (11 89 101 103 107 109))


(can-two-partition? (set/difference (set input) (set '(1 89 101 107 109 113))))
(apply * '(1 89 101 107 109 113))
;; => 11846773891


(can-two-partition? (set/difference (set input) (set '(3 97 101 103 107 109))))
(can-two-partition? (set/difference (set input) (set '(7 83 101 107 109 113))))
(can-two-partition? (set/difference (set input) (set '(7 89 101 103 107 113))))
(apply * '(7 89 101 103 107 113))
;; => 78362605279

(set/difference (set input) (set '(1 89 101 107 109 113)))

(/ (apply + input) 4)
;; => 390

(count (filter #(= (apply + %) 390) (combo/combinations input 4)))

(->> (combo/combinations input 4)
     (filter #(= (apply + %) 390))
     (sort-by #(apply * %))
     (take 10)
     first
     (apply *))
;; => 80393059
