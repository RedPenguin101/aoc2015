(ns aoc2015.scratch
  (:require [clojure.string :as str]))

(def x {"Audi" []
        "BMW" ["X1" "i3"]
        "Fiat" ["500"]
        "Havel" []})

(defn pair-up [[mk mdls]]
  (if (empty? mdls) mk
      (map #(str mk " " %) mdls)))

(defn create-placeholder [x] (->> x (mapcat pair-up) (str/join ", ")))

(create-placeholder x)

(->> x (map pair-up))

"Random leetcode problem https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/"

(def s "(1+(2*3)+((8)/4))+1")

(defn max-depth [s] (->> s (keep {\( 1 \) -1}) (reductions +) (apply max)))

(max-depth s)

"https://leetcode.com/problems/reverse-integer/"

(defn reverse-digits [x]
  (cond-> (->> x Math/abs str reverse (apply str) Long/parseLong)
    (neg? x) -))

(reverse-digits 123)
(reverse-digits 120)
(reverse-digits 0)
(reverse-digits -123)
