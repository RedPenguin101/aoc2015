(ns aoc2015.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :refer [deftest is are]]))

"Day 7: Some Assembly Required
 
 wires and bitwise logic gates.
 wire: id (lowercase letters)
 carries 16bit signal (0-65535)
 
 connection instr: z AND y -> z => connect x and y to an AND gate, connect output to z
 signal intr: 123 -> x
 
 gate types: and or not rshift lshift.
 Emulate a circuit!
 
 what signal is ultimately provided to wire a?
 
 Concepts: A signal is on a wire. 
 
 Need to maintain a state of a wire?
 
 How about a map of wireid->[op, [sources]]
 
 main api point would be (signal circuit wireid) which would run through recursively
 
 Wow, that really didn't work. Recursion was too slow (or wasn't working) 
 and couldn't get memoization working.

 Also tried getting too fancy with stuff, inlining functions - never smart.
 
 Try an iterative approach next"

(defn parses? [a] (try (Long/parseLong a) (catch Exception e nil)))


(comment
  (def example "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i")

  (def example2 "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
lx -> ly")

  "examples of the parsed out I want"
  ["x" 123]
  ["d" "AND" "x" "y"]
  ["d" "AND" "x" "y"]
  ["f" "LSHIFT" "x" 2]
  ["h" "NOT" "x"])

(defn parse-instr [s]
  (let [[a b c :as words] (str/split s #" ")]
    (into [(last words)]
          (cond (and (= 3 (count words))
                     (parses? a))       [(parses? a)]
                (= 3 (count words))     ["DIRECT" a]
                (= "NOT" a)             ["NOT" b]
                :else                   [b a c]))))

(def input (map parse-instr (str/split-lines (slurp "resources/day7input"))))

(defn find-numeric-inputs [input]
  (into {} (filter (fn [[_ x]] (number? x)) input)))

(comment
  (map parse-instr (str/split-lines example2))
  (find-numeric-inputs (map parse-instr (str/split-lines example2)))
  (find-numeric-inputs input))

(defn execute [op args]
  (mod (case op
         "AND" (apply bit-and args)
         "OR" (apply bit-or args)
         "LSHIFT" (apply bit-shift-left args)
         "RSHIFT" (apply bit-shift-right args)
         "NOT" (apply bit-not args)
         "DIRECT" (first args))
       65536))

(defn is-number-or-found [found arg]
  (or (parses? arg) (get found arg nil)))

(defn lookup-and-add [solved [target op & args]]
  (let [found-args (keep #(is-number-or-found solved %) args)]
    (if (or (number? op) (not= (count args) (count found-args)))
      solved
      (assoc solved target (execute op found-args)))))

(defn run
  ([input] (run (find-numeric-inputs input) input 0))
  ([solved input it]
   (cond (> it 10000) :break
         (= (count solved) (count input)) solved
         :else (recur (reduce lookup-and-add solved input)
                      input (inc it)))))

(comment
  (reduce lookup-and-add {"x" 123 "y" 456} '(["x" 123]
                                             ["y" 456]
                                             ["d" "AND" "x" "y"]
                                             ["e" "OR" "x" "y"]
                                             ["f" "LSHIFT" "x" "2"]
                                             ["g" "RSHIFT" "y" "2"]
                                             ["h" "NOT" "x"]
                                             ["i" "NOT" "y"]))

  (run '(["x" 123]
         ["y" 456]
         ["d" "AND" "x" "y"]
         ["e" "OR" "x" "y"]
         ["f" "LSHIFT" "x" "2"]
         ["g" "RSHIFT" "y" "2"]
         ["h" "NOT" "x"]
         ["i" "NOT" "y"]))

  (time (get (run input) "a"))
  ;; => 3176 (in ~250ms)

  (-> (find-numeric-inputs input)
      (assoc "b" 3176)
      (run input 0)
      (get "a")
      (time))
  ;; => 14710 (in ~250ms)
  )

