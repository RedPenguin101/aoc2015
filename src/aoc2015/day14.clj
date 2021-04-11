(ns aoc2015.day14)

"Day 14: Reindeer Olympics
 
 Reindeer can fly at high speeds, but must rest occasionally to recover their energy.
 which of his reindeer is fastest

 Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds.
 
 after exactly 2503 seconds, what distance has the winning reindeer traveled?"

(defn flying? [[_ fly rst] sec]
  (let [x (mod sec (+ fly rst))]
    (< x fly)))

(defn distance [[spd _ _ :as profile] sec]
  (if (flying? profile sec) spd 0))

(defn race [finish profile]
  (reduce + (map #(distance profile %) (range 0 (inc finish)))))

(comment
  (race 1000 [14 10 127]) ;; => 1120
  (race 1000 [16 11 162]) ;; => 1056

  (time (sort (map #(race 2503 %) [[8 8 53] [13 4 49] [20 7 132] [12 4 43] [9 5 38] [10 4 37] [3 37 76] [9 12 97] [37 1 36]])))
  ;; => (2470 2484 2496 2496 2516 2560 2592 2648 2655)
  ;;   in 3ms
  )

(defn point-to-leaders [positions]
  (let [l (apply max positions)]
    (map #(if (= l %) 1 0) positions)))

(defn add-vectors [v1 v2]
  (map + v1 v2))

(comment
  (time (doall (map (fn [sec] (map #(race sec %) [[8 8 53] [13 4 49] [20 7 132] [12 4 43] [9 5 38] [10 4 37] [3 37 76] [9 12 97] [37 1 36]]))
                    (range 0 2504))))
  ;; => 2504 in 3.9ms


  (time (count (map point-to-leaders
                    (map (fn [sec] (map #(race sec %) [[8 8 53] [13 4 49] [20 7 132] [12 4 43] [9 5 38] [10 4 37] [3 37 76] [9 12 97] [37 1 36]]))
                         (range 0 2504)))))
  ;; => 2504 in 2.3 seconds

  (time (reduce add-vectors
                (take 2504 (map point-to-leaders
                                (map (fn [sec] (map #(race sec %) [[8 8 53] [13 4 49] [20 7 132] [12 4 43] [9 5 38] [10 4 37] [3 37 76] [9 12 97] [37 1 36]]))
                                     (range 0 2504))))))
  ;; => (1059 5 887 13 415 0 22 153 1) in 2.3 second
  )
