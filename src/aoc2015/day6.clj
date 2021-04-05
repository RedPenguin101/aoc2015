(ns aoc2015.day6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

"Day 6: Probably a Fire Hazard
 
 lights, 1kx1k grid [x y], instruction: turn on turn off
 
 turn on 0,0 through 999,999
 toggle 0,0 through 999,0

 how many lights are lit after instructions?
 
 Simple representation as set of cooreds, then set operations
 
 API:
 process :: instruction, current-lights, new-lights
 
 say we have a 2x2 grid.
 if current set is #{[1 1] [0 0]} and we turn on #{[1 1] [2 2]}, then result is #{[0 0] [1 1] [2 2]}
 if current set is #{[1 1] [0 0]} and we turn off #{[1 1] [2 2]}, then result is #{[0 0]}
 if current set is #{[1 1] [0 0]} and we toggle #{[1 1] [2 2]}, then result is #{[0 0] [2 2]}
 

 if instruction is on, then union the two sets, if off, then difference the two sets
 if toggle, then it's the exclusive or one, whose name I forget, but is the 
 difference of the intersection and union"

(defn exclusive-union [s1 s2]
  (set/difference (set/union s1 s2) (set/intersection s1 s2)))

(defn process [current [instr new]]
  (case instr
    "turn on" (set/union current new)
    "turn off" (set/difference current new)
    "toggle" (exclusive-union current new)))

(defn new-lights [[x1 y1 x2 y2]]
  (set (for [x' (range x1 (inc x2))
             y' (range y1 (inc y2))]
         [x' y'])))

(defn parse [string]
  [(cond (str/starts-with? string "turn on") "turn on"
         (str/starts-with? string "turn off") "turn off"
         :else "toggle")
   (new-lights (map #(Integer/parseInt %) (re-seq #"\d+" string)))])

(comment
  (def input (map parse (str/split-lines (slurp "resources/day6input"))))
  (time (count) (reduce process #{} input)))