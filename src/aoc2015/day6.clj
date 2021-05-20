(ns aoc2015.day6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

"Day 6: Probably a Fire Hazard
 
 lights, 1kx1k grid [x y], instruction: turn on turn off
 
 instructions:
 turn on 0,0 through 999,999
 toggle 0,0 through 999,0

 how many lights are lit after instructions?
 
 Simple representation as set of coords, then set operations
 
 API:
 process :: instruction, current-lights -> new-lights
 
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
  (time (count (reduce process #{} input))))

"Part2
 lights have individual brightness controls
 turn on mean increase brightness by 1
 turn off means decrease brightness by 1 (minimum of 0)
 toggle means increase brightness by 2
 
 What is the total brightness of all lights"

(defn parse2 [string]
  (let [coords (map #(Long/parseLong %) (re-seq #"\d+" string))]
    (cond (str/starts-with? string "turn on")
          (into [:on] coords)
          (str/starts-with? string "turn off")
          (into [:off] coords)
          (str/starts-with? string "toggle")
          (into [:toggle] coords))))

(defn process2 [m [instr x1 x2 x3 x4]]
  (reduce (fn [M coord]
            (if (M coord)
              (update M coord (case instr
                                :on inc
                                :off #(max 0 (dec %))
                                :toggle #(+ 2 %)))
              (assoc M coord (case instr
                               :on 1
                               :off 0
                               :toggle 2))))
          m
          (new-lights [x1 x2 x3 x4])))

(comment
  (time (reduce + (vals (reduce process2 {} input))))
  ;;"Elapsed time: 39163.715991 msecs"
  ;; 17836115
  )