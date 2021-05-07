(ns aoc2015.day19
  (:require [clojure.string :as str]
            [clojure.set :as set]))

"Day 19: Medicine for Rudolph
 https://adventofcode.com/2015/day/19
 
 custom-made medicine
 constructing molecule
 starting with some input molecule and then doing a series of replacements, one per step
 machine has to be calibrated, determining the number of molecules that can be generated in one step
 "

(defn split-molecules [string]
  (reduce (fn [A c]
            (if (Character/isUpperCase c)
              (conj A (str c))
              (conj (vec (butlast A)) (str (last A) c))))
          []
          string))

(defn replace-molecules [target [a b]]
  (keep (fn [idx] (when (= a (get target idx))
                    (apply str (assoc target idx b))))
        (range (count target))))

(defn step [target replacements]
  (set (mapcat #(replace-molecules (split-molecules target) %) replacements)))

(def input "CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCaCaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMgArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTiBPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnBFArCaFArCaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMgArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRnBPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnBFArCaSiRnTiMgArSiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCaPTiBPTiBCaSiThSiRnMgArCaF")

(def replacements [["Al" "ThF"]
                   ["Al" "ThRnFAr"]
                   ["B" "BCa"]
                   ["B" "TiB"]
                   ["B" "TiRnFAr"]
                   ["Ca" "CaCa"]
                   ["Ca" "PB"]
                   ["Ca" "PRnFAr"]
                   ["Ca" "SiRnFYFAr"]
                   ["Ca" "SiRnMgAr"]
                   ["Ca" "SiTh"]
                   ["F" "CaF"]
                   ["F" "PMg"]
                   ["F" "SiAl"]
                   ["H" "CRnAlAr"]
                   ["H" "CRnFYFYFAr"]
                   ["H" "CRnFYMgAr"]
                   ["H" "CRnMgYFAr"]
                   ["H" "HCa"]
                   ["H" "NRnFYFAr"]
                   ["H" "NRnMgAr"]
                   ["H" "NTh"]
                   ["H" "OB"]
                   ["H" "ORnFAr"]
                   ["Mg" "BF"]
                   ["Mg" "TiMg"]
                   ["N" "CRnFAr"]
                   ["N" "HSi"]
                   ["O" "CRnFYFAr"]
                   ["O" "CRnMgAr"]
                   ["O" "HP"]
                   ["O" "NRnFAr"]
                   ["O" "OTi"]
                   ["P" "CaP"]
                   ["P" "PTi"]
                   ["P" "SiRnFAr"]
                   ["Si" "CaSi"]
                   ["Th" "ThCa"]
                   ["Ti" "BP"]
                   ["Ti" "TiTi"]
                   ["e" "HF"]
                   ["e" "NAl"]
                   ["e" "OMg"]])

(comment
  (time (count (step input replacements)))
  ;;"Elapsed time: 260.700883 msecs"
  535)

(defn search [path seen replacements target it]
  (let [[steps molecule] (first path)
        new-mols (remove seen (step molecule replacements))]
    (cond (> it 10000) :break
          (= target molecule) steps
          :else
          (recur
           (into (vec (rest path)) (map #(vector (inc steps) %) new-mols))
           (set/union seen (set new-mols))
           replacements target (inc it)))))

(comment (search [[0 "e"]] #{} replacements input 0))

(into '(1 2 3) '(4 5 6))

(comment
  (search [[0 "e"]]
          #{}
          [["e" "H"] ["e" "O"] ["H" "HO"] ["H" "OH"] ["O" "HH"]]
          "HOHOHO"
          0)
  ;; => 6

  (search [[0 "e"]]
          #{}
          [["e" "H"] ["e" "O"] ["H" "HO"] ["H" "OH"] ["O" "HH"]]
          "HOH"
          0)
  ;; => 3

  "Too slow though! Maybe work backwards instead, simplifying")

(defn replace-individually [s match replacement]
  (let [re (re-pattern (str "(?=" match ")"))
        groups (str/split s re)]
    (for [idx (range (count groups))
          :when (str/starts-with? (get groups idx) match)]
      (apply str (update groups idx #(str/replace-first % match replacement))))))

(defn replace-molecules2 [target [a b]]
  (replace-individually target a b))

(defn not-e-but-contains-e? [s]
  (and (not (= "e" s)) (re-find #"e" s)))

(set (sort-by count (remove not-e-but-contains-e? (mapcat #(replace-molecules2 "HOHOHO" %) [["H" "e"] ["O" "e"] ["HO" "H"] ["OH" "H"] ["HH" "O"]]))))

(not-e-but-contains-e? "Hello")

(defn search2 [path seen replacements it]
  (let [[steps molecule] (first path)
        new-mols (set (sort-by count (remove not-e-but-contains-e? (mapcat #(replace-molecules2 molecule %) replacements))))]
    (cond (> it 10000) :break
          (= "e" molecule) steps
          :else
          (recur
           (into (vec (rest path)) (map #(vector (inc steps) %) new-mols))
           (set/union seen (set new-mols))
           replacements
           (inc it)))))

(search2 [[0 "HOH"]] #{} [["H" "e"] ["O" "e"] ["HO" "H"] ["OH" "H"] ["HH" "O"]] 0)
(search2 [[0 "HOHOHO"]] #{} [["H" "e"] ["O" "e"] ["HO" "H"] ["OH" "H"] ["HH" "O"]] 0)

(def repl-reversed (map (fn [[x y]] [y x]) replacements))

(comment
  (search2 [[0 input]] #{} repl-reversed 0)

  "Still too slow :(")