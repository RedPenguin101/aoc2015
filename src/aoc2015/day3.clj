(ns aoc2015.day3)

"Day3: Perfectly Spherical Houses in a Vacuum
 
 2d grid, house, move (NSEW), repeats
 
 HOW MANY HOUSES RECEIVE AT LEAST ONE PRESENT"

(defn move [houses mv]
  (let [[x y] (first houses)]
    (case mv
      \^ (conj houses [x (inc y)])
      \v (conj houses [x (dec y)])
      \> (conj houses [(inc x) y])
      \< (conj houses [(dec x) y]))))

(comment
  (count (set (reduce move '([0 0]) ">")))
  (count (set (reduce move '([0 0]) "^>v<")))
  (count (set (reduce move '([0 0]) "^v^v^v^v^v")))
  (count (set (reduce move '([0 0]) (slurp "resources/day3input"))))

  "Where your have two santas taking alternate instructions"

  (->> (slurp "resources/day3input")
       (partition 2)
       (apply map vector)
       (map #(set (reduce move '([0 0]) %)))
       (apply clojure.set/union)
       count))
