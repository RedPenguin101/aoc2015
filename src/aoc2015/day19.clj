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

(def replacements [["H" "HO"]
                   ["H" "OH"]
                   ["O" "HH"]])

["H" "O" "O"]

(defn split-molecules [string]
  (reduce (fn [A c]
            (if (Character/isUpperCase c)
              (conj A (str c))
              (conj (vec (butlast A)) (str (last A) c))))
          []
          string))

(split-molecules "HaOHa")

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
  #_(println (first path))
  (when (zero? (mod it 1000)) (println it))
  (let [[steps molecule] (first path)
        new-mols (remove seen (step molecule replacements))]
    (cond (> it 10000) :break
          (= target molecule) steps
          :else
          (recur
           (concat (rest path) (map #(vector (inc steps) %) new-mols))
           (set/union seen (set new-mols))
           replacements target (inc it)))))

(comment (search [[0 "e"]] #{} replacements input 0))

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
  )


