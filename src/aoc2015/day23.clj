(ns aoc2015.day23
  (:require [clojure.string :as str]))

"Day 23: Opening the Turing Lock
 
 first computer
 two registers: a and b, non-neg int, init with 0
 six instructions
 * hlf r:         sets register r to half its current value, then continues with the 
                  next instruction.
 * tpl r:         sets register r to triple its current value, then continues with the
                  next instruction.
 * inc r:         increments register r, adding 1 to it, then continues with the next 
                  instruction.
 * jmp offset:    is a jump; it continues with the instruction offset away relative 
                  to itself.
 * jie r, offset: is like jmp, but only jumps if register r is even (jump if even).
 * jio r, offset: is like jmp, but only jumps if register r is 1 (jump if one, not odd).

 offset written with a prefix + or - : direction

 The program exits when it tries to run an instruction beyond the ones defined.
 
 example prog:
 inc a
 jio a, +2
 tpl a
 inc a
 
 What is the value in register b when the program in your puzzle input is finished executing?"

(defn step [[pointer registers instructions]]
  (let [[instr x y] (get instructions pointer)]
    (case instr
      :hlf [(inc pointer) (update registers x #(long (Math/floor (/ % 2)))) instructions]
      :tpl [(inc pointer) (update registers x * 3) instructions]
      :inc [(inc pointer) (update registers x inc) instructions]

      :jmp [(+ pointer x) registers instructions]

      :jie [(if (even? (x registers)) (+ y pointer) (inc pointer)) registers instructions]
      :jio [(if (= 1 (x registers)) (+ y pointer) (inc pointer)) registers instructions])))

(defn run [[pointer registers instructions :as state]]
  (if (get instructions pointer) (recur (step state)) registers))

(defn parse-line [line]
  (let [instr (keyword (subs line 0 3))]
    (cond
      (#{:hlf :tpl :inc} instr) [instr (keyword (subs line 4))]
      (#{:jmp} instr)           [instr (Long/parseLong (subs line 4))]
      (#{:jie :jio} instr)      [instr (keyword (subs line 4 5)) (Long/parseLong (subs line 7))])))

(def instr (mapv parse-line (str/split-lines (slurp "resources/day23input"))))

(comment
  (run [0 {:a 0 :b 0} instr])
  ;; => {:a 1, :b 307}

  (run [0 {:a 1 :b 0} instr])
  ;; => {:a 1, :b 160}
  )
