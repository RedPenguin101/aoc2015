(ns aoc2015.day19-2
  (:require [clojure.string :as str]))

;; this is pretty dumb implementation, since it assumes the correct and
;; only path to the solution is to take the 'smallest' molecule at each
;; step. This happens to be the case here, but it certainly might not have
;; been!

(def input "CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCaCaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMgArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTiBPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnBFArCaFArCaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMgArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRnBPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnBFArCaSiRnTiMgArSiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCaPTiBPTiBCaSiThSiRnMgArCaF")

(def replacements [["Al" "ThF"] ["Al" "ThRnFAr"] ["B" "BCa"] ["B" "TiB"] ["B" "TiRnFAr"] ["Ca" "CaCa"] ["Ca" "PB"] ["Ca" "PRnFAr"] ["Ca" "SiRnFYFAr"] ["Ca" "SiRnMgAr"] ["Ca" "SiTh"] ["F" "CaF"] ["F" "PMg"] ["F" "SiAl"] ["H" "CRnAlAr"] ["H" "CRnFYFYFAr"] ["H" "CRnFYMgAr"] ["H" "CRnMgYFAr"] ["H" "HCa"] ["H" "NRnFYFAr"] ["H" "NRnMgAr"] ["H" "NTh"] ["H" "OB"] ["H" "ORnFAr"] ["Mg" "BF"] ["Mg" "TiMg"] ["N" "CRnFAr"] ["N" "HSi"] ["O" "CRnFYFAr"] ["O" "CRnMgAr"] ["O" "HP"] ["O" "NRnFAr"] ["O" "OTi"] ["P" "CaP"] ["P" "PTi"] ["P" "SiRnFAr"] ["Si" "CaSi"] ["Th" "ThCa"] ["Ti" "BP"] ["Ti" "TiTi"] ["e" "HF"] ["e" "NAl"] ["e" "OMg"]])
(def rep-match (map #(update % 1 re-pattern) replacements))

(defn find-simplifications [string replacements]
  (keep (fn [[new old]] (when (re-find old string) (str/replace-first string old new)))
        replacements))

(defn next-mol [steps string]
  (first (sort-by (comp count second) < (map vector (repeat (inc steps)) (find-simplifications string rep-match)))))

(defn search [[steps string]]
  (if (= "e" string) steps
      (recur (next-mol steps string))))

(comment
  (time (search [0 input])))