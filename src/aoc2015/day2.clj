(ns aoc2015.day2)

"Day 2: I was told there would be no math
 
 wrapping paper, order, dimensions (lxwxh)
 
 box surface area = 2*(lw+wh+hl)"

(defn smallest-side [l w h] (apply * (take 2 (sort [l w h]))))
(defn paper [[l w h]] (+ (smallest-side l w h) (* 2 (+ (* l w) (* w h) (* h l)))))

(smallest-side 2 3 4)
(paper [2 3 4])
(paper [1 1 10])

(apply + (map paper (partition 3 (map #(Integer/parseInt %) (re-seq #"\d+" (slurp "resources/day2input"))))))

(defn volume [l w h] (* l w h))

(defn smallest-perim [l w h] (* 2 (apply + (take 2 (sort [l w h])))))

(defn ribbon [[l w h]] (+ (volume l w h) (smallest-perim l w h)))

(ribbon [2 3 4])
(ribbon [1 1 10])

(apply + (map ribbon (partition 3 (map #(Integer/parseInt %) (re-seq #"\d+" (slurp "resources/day2input"))))))
