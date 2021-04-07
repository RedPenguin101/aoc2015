(ns aoc2015.day12
  (:require [clojure.data.json :as json]))

"Day 12: JSAbacusFramework.io
 
 JSON document which contains a variety of things: arrays ([1,2,3]), objects ({\"a\":1, \"b\":2}), numbers, and strings.
 
 find all of the numbers throughout the document and add them together.
 
 Simple tree walk?"

(defn sum-nums [tree]
  (let [tree (if (map? tree) (vals tree) tree)]
    (reduce (fn [n subtree]
              (cond (map? subtree) (+ n (sum-nums (vals subtree)))
                    (coll? subtree) (+ n (sum-nums subtree))
                    (number? subtree) (+ n subtree)
                    :else n))
            0
            tree)))

(defn contains-red [s]
  (some  #{"red"} s))

(defn sum-nums-no-red [tree]
  (reduce (fn [n subtree]
            (cond (and (map? subtree) (contains-red (vals subtree))) n
                  (map? subtree) (+ n (sum-nums-no-red (vals subtree)))
                  (coll? subtree) (+ n (sum-nums-no-red subtree))
                  (number? subtree) (+ n subtree)
                  :else n))
          0
          tree))

(comment
  (sum-nums [1 2 3 [4 5] {:a 4 :b 5}])
  (sum-nums [1,2,3])
  (sum-nums {"a" 2 "b" 4})
  (sum-nums [[[3]]])
  (sum-nums {"a" {"b" 4},"c" -1})
  (sum-nums {"a" [-1,1]})
  (sum-nums [-1,{"a" 1}])
  (sum-nums [])
  (sum-nums {})
  (time (sum-nums (json/read-str (slurp "resources/day12input"))))
  ;; => 119433

  (sum-nums-no-red [1,2,3])
  (sum-nums-no-red [1,{"c" "red","b" 2},3])
  (sum-nums-no-red {"d" "red","e" [1,2,3,4],"f" :5})
  (sum-nums-no-red [1,"red",5])
  (time (sum-nums-no-red (vals (json/read-str (slurp "resources/day12input")))))
  ;; => 68466
  )
