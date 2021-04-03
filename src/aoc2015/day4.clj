(ns aoc2015.day4)

"Day 4: The Ideal Stocking Stuffer
 
 mining, AdventCoins, MD5 hashes, hexadecimal five leading 0s, input = ckczppom+decimal number
 
 FIND LOWEST POSSIBLE INTEGER THAT PRODUCES SUCH A HASH"

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(comment
  (md5 "abcdef609043")
  (md5 "pqrstuv1048970")
;; => "000006136ef2ff3b291c85725f17325c"

  (time (first (drop-while (comp nil? second) (map #(vector % (re-find #"^00000" (md5 (str "ckczppom" %)))) (range)))))
;; => [117946 "00000"]

  (time (first (drop-while (comp nil? second) (map #(vector % (re-find #"^000000" (md5 (str "ckczppom" %)))) (range))))))

