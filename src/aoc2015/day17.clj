(ns aoc2015.day17
  (:require [clojure.math.combinatorics :as combo]))

"Day 17: No Such Thing as Too Much
 
 150 liters
 move it into smaller containers
 how many different combinations of containers can EXACTLY fit all 150
 
 Seems like a recursive solution would work
 
 ways-to-fit(total, containers) = map (ways-to-fit (total - size) others) containers
 
 But would be slow I think. And have to handle duplicates: (20,5) is the same as (5,20)"

(def input (sort [50 44 11 49 42 46 18 32 26 40 21 7 18 43 10 47 36 24 22 40]))
(def example (sort [20 15 10 5 5]))

(defn ways-to-fit [liquid [this & others]]
  (cond (or (not this) (neg? liquid)) 0
        (= liquid this) (+ 1 (ways-to-fit liquid others))
        :else (+ (ways-to-fit liquid others)
                 (ways-to-fit (- liquid this) others))))

(comment
  (time (ways-to-fit 25 example))
  ;; "Elapsed time: 0.364193 msecs"
  ;; => 4 )
  (time (ways-to-fit 150 input))
  ;; "Elapsed time: 20.622151 msecs"
  ;; => 654
  )

(defn ways-to-fit2 [solutions current liquid [nxt & others]]
  (let [new (conj current nxt)]
    (cond (or (not nxt) (neg? liquid)) nil

          (= liquid nxt)
          (concat (conj solutions new)
                  (ways-to-fit2 [] current liquid others))

          :else (concat solutions
                        (ways-to-fit2 [] current liquid others)
                        (ways-to-fit2 [] new (- liquid nxt) others)))))

(comment
  (remove empty? (ways-to-fit2 [] [] 25 example))
  ;; => ([10 15] [5 20] [5 20] [5 5 15])

  (->> (ways-to-fit2 [] [] 150 input)
       (filter #(= 4 (count %)))
       count)
  ;; => 57 

  (time (->> (ways-to-fit2 [] [] 150 input)
             (group-by count)
             sort
             first
             second
             count))
  ;; => 57

  (ways-to-fit2 [] [] 25 example)
  (ways-to-fit2 [] [] 150 input))