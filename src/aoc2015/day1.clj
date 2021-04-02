(ns aoc2015.day1
  (:require [clojure.test :refer [deftest are]]))

"Day1: Not quite lisp
 
 apartment building, floor, (= up 1 floor )=down"

(defn move [floor nxt]
  (case nxt
    \( (inc floor)
    \) (dec floor)))

(comment
  (reduce move 0 (slurp "resources/day1input"))
  (count (take-while #(>= % 0) (reductions move 0 "()())")))
  (count (take-while #(>= % 0) (reductions move 0 (slurp "resources/day1input")))))

(deftest t
  (are [s result] (= result (reduce move 0 s))
    "(())" 0
    "()()" 0
    "(((" 3
    "(()(()(" 3
    "))(((((" 3
    "())" -1
    "))(" -1
    ")))" -3
    ")())())" -3))
