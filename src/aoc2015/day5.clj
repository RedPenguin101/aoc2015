(ns aoc2015.day5
  (:require [clojure.set :as set]
            [clojure.string :as str]))

"Day 5: Doesn't He Have Intern-Elves For This?
 
 nice, naighty strings
 nice: contains >=3 vowels, contains at least one letter twice in a row, doesn't contain ab,cd,pq,xy"

(defn vowel-count [n string]
  (>= (count (filter (set "aeiou") string)) n))

(defn in-row [n string]
  (some #(>= (count %) n) (partition-by identity string)))

(defn not-forbidden? [string]
  (not (some #(str/includes? string %) ["ab" "cd" "pq" "xy"])))

(defn nice-string [string]
  ((every-pred #(vowel-count 3 %) #(in-row 2 %) not-forbidden?)
   string))

(comment
  (nice-string "ugknbfddgicrmopn")
  (nice-string "aaa")
  (nice-string "jchzalrnumimnmhp")
  (nice-string "haegwjzuvuyypxyu")
  (nice-string "dvszwmarrgswjxmb")
  (count (str/split-lines (slurp "resources/day5input")))
  (count (filter nice-string (str/split-lines (slurp "resources/day5input")))))

"It contains a pair of any two letters that appears at least twice in the string without overlapping, 
 like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).

 It contains at least one letter which repeats with exactly one letter between them, like xyx, 
 abcdefeghi (efe), or even aaa.
"

(defn repeat-with-gap? [string]
  (some #(= (first %) (last %)) (partition 3 1 string)))

(defn two-pair-no-overlap [pairs]
  (cond (empty? pairs) false
        (some #(= (first pairs) %) (drop 2 pairs)) true
        :else (recur (rest pairs))))

(def nice-string2
  (every-pred repeat-with-gap? #(two-pair-no-overlap (partition 2 1 %))))

(comment
  (nice-string2 "qjhvhtzxzqqjkmpb")
  (nice-string2 "xxyxx")
  (nice-string2 "uurcxstgmygtbstg")
  (nice-string2 "ieodomkazucvgmuy")
  (count (filter nice-string2 (str/split-lines (slurp "resources/day5input")))))
