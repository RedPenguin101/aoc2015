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