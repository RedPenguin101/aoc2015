(ns aoc2015.day18
  (:require [clojure.set :as set]
            [clojure.string :as str]))

"Day 18: Like a GIF For Your Yard
 
 100x100 grid.
 initial configuration (your puzzle input). 
 A # means on, and a . means off.
 steps. each step decides the next configuration based on the current one
 eight lights adjacent to it (edge always considered off)

 if on, and 2/3 neighbours are on -> on
 if off and 3 neighbors are on -> on

 current | <2  | 2   | 3  | >3
 on      | off | on  | on | off
 off     | off | off | on | off

 how many lights are on after 100 steps?

 Pretty standard cellular automata problem.

 HI:
 step: current-grid -> next-grid

 MED:
 next-state: current-state, neighbour-states -> new-state
 neigbours-on: grid, coord -> on-neigbours (int)

 LOW:
 neighbours: coord -> coords

 State representation: set of coords which are 'on' 
 (guess p2 will have non-boolean representation of state tho, so will have to
 change)
 "

(defn step [neighbour-fn test-fn]
  (fn [on-coords]
    (let [space (set/union (set (mapcat neighbour-fn on-coords)) on-coords)]
      (set (filter #(test-fn on-coords %) space)))))

(defn neighbours [limit]
  (fn [[x y]]
    (set (for [x' (range (dec x) (+ 2 x))
               y' (range (dec y) (+ 2 y))
               :when (not (and (= x' x) (= y' y)))
               :when (<= 0 x' (dec limit))
               :when (<= 0 y' (dec limit))]
           [x' y']))))

(defn neighbours-on [limit on-coords coord]
  (count (set/intersection on-coords ((neighbours limit) coord))))

(defn on-next? [limit]
  (fn [on-coords coord]
    (let [on-neighbours (neighbours-on limit on-coords coord)]
      #_(println {:on-neighbours [coord on-neighbours]})
      (cond
        (= 3 on-neighbours) true
        (= 2 on-neighbours) (if (on-coords coord) true false)
        :else false))))

(def example ".#.#.#
...##.
#....#
..#...
#.#..#
####..")

(defn parse [grid-str]
  (set (remove nil? (apply concat (map-indexed (fn [y row]
                                                 (map-indexed (fn [x c]
                                                                (when (= \# c)
                                                                  [x y]))
                                                              row))
                                               (str/split-lines grid-str))))))

(def input (parse (str/trim-newline (slurp "resources/day18input"))))

(defn print-coords [coords size]
  (print
   (str/join "\n"
             (map #(apply str %)
                  (partition size (for [y (range 0 size)
                                        x (range 0 size)]
                                    (if (coords [x y]) \# \.)))))))


(comment
  (let [stepfn (step (neighbours 100) (on-next? 100))]
    (time (count (last (take 101 (iterate stepfn input))))))
  ;; "Elapsed time: 3992.105344 msecs"
  ;; 1061
  )

(defn on-next-corner? [limit]
  (fn [on-coords coord]
    (let [on-neighbours (neighbours-on limit on-coords coord)]
      #_(println {:on-neighbours [coord on-neighbours]})
      (cond
        (every? #{0 (dec limit)} coord) true
        (= 3 on-neighbours) true
        (= 2 on-neighbours) (if (on-coords coord) true false)
        :else false))))

(comment
  (let [stepfn (step (neighbours 6) (on-next-corner? 6))]
    (-> (parse "##.#.#\n...##.\n#....#\n..#...\n#.#..#\n####.#")
        stepfn
        stepfn
        stepfn
        stepfn
        stepfn
        (print-coords 6)))

  (def input2 (conj input [0 0] [0 99] [99 0] [99 99]))

  (let [stepfn (step (neighbours 100) (on-next-corner? 100))]
    (time (count (last (take 101 (iterate stepfn input2))))))
  ;; "Elapsed time: 3808.694427 msecs"
  ;;  1006
  )

"Some pretty ugly stuff here - most egregiously the inconsisent use of neighbour
 fn - passed as param in step, but used inline in neigbours-on!
 should revist"