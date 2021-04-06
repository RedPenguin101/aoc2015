(ns aoc2015.day11)

"Day 11: Corporate Policy
 
 Santa's previous password expired
 method of coming up with a password based on the previous one: incrementing his old password string repeatedly until it is valid
 incrementing: xx -> xy -> xz -> ya
 
 passwords must be exactly eight lowercase letters
 must include one increasing straight of at least three letters, like abc, bcd, cde
 may not contain the letters i, o, or l
 must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz
 "

(def letters (mapv str "abcdefghijklmnopqrstuvwxyz"))
(def successor (zipmap letters (into (vec (rest letters)) ["a"])))
(def straights (set (partition 3 1 letters)))

(defn increment [ltrs]
  (cond (empty? ltrs) []
        (not= (last ltrs) "z") (conj (subvec ltrs 0 (dec (count ltrs))) (successor (last ltrs)))
        :else (conj (increment (subvec ltrs 0 (dec (count ltrs)))) (successor (last ltrs)))))

(comment
  (increment (mapv str "a"))
  ;; => ["b"]
  (increment (mapv str "ab"))
  ;; => ["a" "c"]
  (increment (mapv str "z"))
  ;; => ["a"]
  (increment (mapv str "az"))
  ;; => ["b" "a"]
  (increment (mapv str "abcdefgh"))
  ;; => ["a" "b" "c" "d" "e" "f" "g" "i"]
  )

(defn straight? [ltrs]
  (some straights (partition 3 1 ltrs)))

(defn no-forbidden? [ltrs]
  (not (some #{"i" "o" "l"} ltrs)))

(partition 2 1 (mapv str "aaallmmn"))

(defn two-pair? [ltrs]
  (>= (count (filter #(or (= 2 (count %)) (>= (count %) 4)) (partition-by identity ltrs))) 2))

(def valid? (every-pred straight? no-forbidden? two-pair?))

(defn next-pw [current]
  (apply str (first (drop-while (comp not valid?) (drop 1 (iterate increment (mapv str current)))))))

(comment
  (time (next-pw "abcdefgh"))
  ;; => "abcdffaa" 122.731807 msecs
  (time (next-pw "ghijklmn"))
  ;; => "ghjaabcc" 17636.47632 msecs
  (time (next-pw "hepxcrrq"))
  ;; => "hepxxyzz" 2050.234252 msecs
  (time (next-pw "hepxxyzz"))
  ;; => "heqaabcc" 5100.782654 msecs
  )

(comment
  (valid? (mapv str "hijklmmn"))
  (valid? (mapv str "abbceffg"))
  (valid? (mapv str "abbcegjk"))
  (valid? (mapv str "abcdffaa"))
  (valid? (mapv str "ghjaabcc")))
