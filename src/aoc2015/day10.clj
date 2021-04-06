(ns aoc2015.day10
  (:require [clojure.test :refer [deftest is are]]))

"Day 10: Elves Look, Elves Say
 
 take turns making sequences
 previous sequence and using that reading as the next sequence
 211 is read as one two, two ones, which becomes 1221 (1 2, 2 1s)
 "

(defn look-say [xs]
  (mapcat (fn [grp]
            [(count grp) (first grp)])
          (partition-by identity xs)))

(comment
  (time (count (last (take 41 (iterate look-say [1 1 1 3 1 2 2 1 1 3])))))
  ;; => 360154 (in 671ms)
  (time (count (last (take 51 (iterate look-say [1 1 1 3 1 2 2 1 1 3])))))
  ;; => 5103798 (in 8.7s)
  )

(deftest t
  (are [in out] (= out (look-say in))
    [1] [1 1]
    [1 1] [2 1]
    [2 1] [1 2 1 1]
    [1 2 1 1] [1 1 1 2 2 1]
    [1 1 1 2 2 1] [3 1 2 2 1 1]))