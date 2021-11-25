(ns aoc2015.day8
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is are]]))

(def examples (str/split-lines (slurp "resources/day8example")))
(def input (str/split-lines (slurp "resources/day8input")))

(defn decode ([in] (decode [] (butlast (rest in))))
  ([out [x & xs]]
   (cond (not x) (str/join out)
         (and (= \\ x) (= \x (first xs)))
         (recur (conj out \X) (drop 3 xs))
         (= \\ x) (recur (conj out (first xs)) (rest xs))
         :else (recur (conj out x) xs))))

(defn encode ([in] (encode [] in))
  ([out [x & xs]]
   (cond (not x) (str/join (conj (into [\"] out) \"))
         (#{\" \\} x) (recur (conj out \\ x) xs)
         :else        (recur (conj out x) xs))))

(comment
  (map decode examples)

  (- (apply + (map count examples))
     (apply + (map (comp count decode) examples)))
  ;; => 12

  (- (apply + (map count input))
     (apply + (map (comp count decode) input)))
  ;; => 1333

  (- (apply + (map (comp count encode) examples))
     (apply + (map count examples)))
  ;; => 19
  (- (apply + (map (comp count encode) input))
     (apply + (map count input)))
  ;; => 2046
  )

(deftest t
  (are [lits string] (= lits (count (decode string)))
    0 "\"\""
    3 "\"abc\""
    7 "\"aaa\\\"aaa\""
    1 "\"\\x27\""
    17 "\"\\\"msoytqimx\\\\tbklqz\"")
  (is (= 12 (- (apply + (map count examples))
               (apply + (map (comp count decode) examples))))
      (= 1333 (- (apply + (map count input))
                 (apply + (map (comp count decode) input)))))

  (are [enc string] (= enc (count (encode string)))
    6 "\"\""
    9 "\"abc\""
    16 "\"aaa\\\"aaa\""
    11 "\"\\x27\"")
  (is (= 19 (- (apply + (map (comp count encode) examples))
               (apply + (map count examples)))))

  (is (= 2046 (- (apply + (map (comp count encode) input))
                 (apply + (map count input))))))
