(ns aoc2015.day22
  (:require [clojure.test :refer [deftest is]]
            [clojure.set :as set]))

"Day 22: Wizard Simulator 20XX
 
 Wizard
 Same turn format as day 21, same win condition - 0 HP loses
 Or lose if you can't cast a spell
 damage still floored at 1
 Spells cost mana
 
 Magic Missile 
   53 mana.
   does 4 damage.
 Drain 
   73 mana. 
   does 2 damage 
   heals 2 hit points.
 Shield 
   113 mana. 
   armor increased 7 
   for 6 turns.
 Poison 
   173 mana. 
   3 damage per turn.
   for 6 turns
 Recharge 
   229 mana. 
   +101 mana
   for 5 turns
 
 Effects apply at the start of both the player's turns and the boss' turns
   
start with 50 hit points and 500 mana points
Boss: Hit Points: 71 Damage: 10
   
What is the least amount of mana you can spend and still win the fight? 
(Do not include mana recharge effects as spending negative mana.)
"

"Don't think there's a way to solve without simulating turns. Search algorithm maybe?
 state->spell->state, where state are the player and boss

 Probably need a better model of the players that includes which effects are active on them"

'[hp armor mana active-effects]

"Concept inventory

 * won? lost? :: state -> bool
 * turn :: state, spell -> state

 * attack :: state -> state
 * apply-effects :: state -> state
 * effect-expire :: effects -> effects 
 * has-effect? :: state, effect -> bool

 * spell probably want a function for each
 * effect tuple of name, turns.
 * player [hp mana effects]
 * opponent [hp damage effects]
 * 
 
  You cannot cast a spell that would start an effect which is already active. However, effects can be started on the same turn they end."

(def cost {:magic-missile 53
           :drain         73
           :shield        113
           :poison        173
           :recharge      229})

(def effect-length {:shield   6
                    :poison   6
                    :recharge 5})

;; Casting fns: takes a state and returns the state with the effect of the casting

(defn add-effect [state spell]
  (if (contains? effect-length spell)
    (update state :effects conj [spell (effect-length spell)])
    state))

(defn cast-spell [state spell]
  (-> (case spell
        :magic-missile (update-in state [:boss :hp] - 4)
        :drain         (-> state (update-in [:boss :hp] - 2) (update-in [:player :hp] + 2))
        state)
      (update-in [:player :mana] - (cost spell))
      (add-effect spell)))

;; Fns relating to applying the effects of active spells

(defn active-effects [state]        (map first (get state :effects)))
(defn has-effect?    [state effect] (some #{effect} (active-effects state)))

(defn tick-effects-timers [state]
  (update state :effects #(keep (fn [[effect timer]] (when (> timer 1) [effect (dec timer)])) %)))

(defn apply-effects [state]
  (cond-> state
    (has-effect? state :poison)   (update-in [:boss :hp] - 3)
    (has-effect? state :recharge) (update-in [:player :mana] + 101)))

;; taking turns and checking if you've won or lost

(defn boss-attack [state]
  (if (> (:hp (:boss state)) 0)
    (update-in state [:player :hp] - (- (get-in state [:boss :damage]) (if (has-effect? state :shield) 7 0)))
    state))

(defn turn [state spell]
  (-> state
      ;; uncomment this for hard mode
      ;;(update-in [:player :hp] - 1)
      apply-effects tick-effects-timers (cast-spell spell)
      apply-effects tick-effects-timers boss-attack))

(defn won?  [state] (<= (get-in state [:boss :hp]) 0))
(defn lost? [state] (<= (get-in state [:player :hp]) 0))

;; Search algorithm
;; Paths are tuples of [total-mana-cost, all-spells-cast, state]

(defn legal-spells [state]
  (set/difference (set (keep (fn [[spell cost]] (when (>= (get-in state [:player :mana]) cost) spell)) cost))
                  (set (active-effects state))))

(defn next-path [[total-mana-cost spells-cast state] spell seen]
  (let [next-state (turn state spell)]
    (when (not (or (seen next-state) (lost? next-state)))
      [(+ total-mana-cost (spell cost)) (conj spells-cast spell)  next-state])))

(defn search
  ([start-state] (search #{} [[0 [] start-state]]))
  ([seen paths]
   (let [[total-mana-cost spells-cast state] (first paths)
         next-paths (keep #(next-path (first paths) % seen) (legal-spells (-> state apply-effects tick-effects-timers)))]
     (if (won? state) [total-mana-cost spells-cast]
         (recur (set/union seen (set (map last next-paths)))
                (sort-by first (into (rest paths) next-paths)))))))

(comment
  "Part 1"
  (search {:player {:hp 50 :mana 500} :boss {:hp 71 :damage 10}})
  ;; => [1824 [:recharge :poison :shield :recharge :poison :shield :recharge :poison :shield :magic-missile :poison :magic-missile]]

  ;; part 2 - have to change turn fn
  (search {:player {:hp 50 :mana 500} :boss {:hp 71 :damage 10}})
  ;; => [1937 [:shield :recharge :poison :shield :recharge :poison :shield :recharge :poison :shield :magic-missile :poison :magic-missile]]
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest has-effect-test
  (is (not (has-effect? {:player {:hp 50 :mana 500}
                         :boss   {:hp 71 :damage 10}}
                        :shield)))
  (is (has-effect? {:player {:hp 50 :mana 500}
                    :boss   {:hp 71 :damage 10}
                    :effects [[:shield 6]]}
                   :shield))
  (is (has-effect? {:player {:hp 50 :mana 500}
                    :boss   {:hp 71 :damage 10}
                    :effects [[:shield 5]]}
                   :shield))
  (is (has-effect? {:player {:hp 50 :mana 500}
                    :boss   {:hp 71 :damage 10}
                    :effects [[:shield 5]]}
                   :shield)))

(deftest boss-attack-test
  (is (= 40 (get-in (boss-attack {:player {:hp 50 :mana 500}
                                  :boss   {:hp 71 :damage 10}})
                    [:player :hp])))
  (is (= 47 (get-in (boss-attack {:player {:hp 50 :mana 500}
                                  :boss   {:hp 71 :damage 10}
                                  :effects [[:shield 4]]})
                    [:player :hp]))))

(deftest effects-tick
  (is (= [:shield :poison]
         (active-effects (tick-effects-timers
                          {:player {:hp 50 :mana 500}
                           :boss   {:hp 71 :damage 10}
                           :effects [[:shield 5] [:poison 5]]}))))
  (is (empty? (active-effects (tick-effects-timers
                               {:player {:hp 50 :mana 500}
                                :boss   {:hp 71 :damage 10}
                                :effects [[:shield 1] [:poison 1]]})))))

(deftest apply-effects-test
  (let [state {:player {:hp 50 :mana 500}
               :boss   {:hp 71 :damage 10}}]
    (is (= state (apply-effects state)))
    (is (= {:hp 68, :damage 10} (:boss (apply-effects (cast-spell state :poison)))))))

(deftest turn-test
  (is (= {:player {:hp 2 :mana 77}
          :boss   {:hp 10 :damage 8}
          :effects [[:poison 5]]}
         (turn {:player {:hp 10 :mana 250}
                :boss   {:hp 13 :damage 8}}
               :poison)))
  (is (= {:player {:hp 2 :mana 24}
          :boss   {:hp 0 :damage 8}
          :effects [[:poison 3]]}
         (turn {:player {:hp 2 :mana 77}
                :boss   {:hp 10 :damage 8}
                :effects [[:poison 5]]}
               :magic-missile)))

  (is (= {:player {:hp 2 :mana 122}
          :boss   {:hp 14 :damage 8}
          :effects [[:recharge 4]]}
         (turn {:player {:hp 10 :mana 250}
                :boss   {:hp 14 :damage 8}}
               :recharge)))

  (is (= {:player {:hp 1 :mana 211}
          :boss   {:hp 14 :damage 8}
          :effects [[:shield 5] [:recharge 2]]}
         (turn {:player {:hp 2 :mana 122}
                :boss   {:hp 14 :damage 8}
                :effects [[:recharge 4]]}
               :shield)))

  (is (= {:player {:hp 2 :mana 340}
          :boss   {:hp 12 :damage 8}
          :effects [[:shield 3]]}
         (turn {:player {:hp 1 :mana 211}
                :boss   {:hp 14 :damage 8}
                :effects [[:shield 5] [:recharge 2]]}
               :drain)))

  (is (= {:player {:hp 1, :mana 167}
          :boss {:hp 9, :damage 8}
          :effects '([:poison 5] [:shield 1])}
         (turn {:player {:hp 2 :mana 340}
                :boss   {:hp 12 :damage 8}
                :effects [[:shield 3]]}
               :poison)))

  (is (= -1 (get-in (turn {:player {:hp 1, :mana 167}
                           :boss {:hp 9, :damage 8}
                           :effects '([:shield 1] [:poison 5])}
                          :magic-missile)
                    [:boss :hp]))))

(deftest legal-spells-test
  (is (empty? (legal-spells {:player {:mana 50}})))
  (is (= #{:magic-missile} (legal-spells {:player {:mana 53}})))
  (is (= #{:magic-missile :drain :shield} (legal-spells {:player {:mana 120}})))
  (is (= #{:magic-missile :drain :shield :poison :recharge} (legal-spells {:player {:mana 250}})))
  (is (= #{:magic-missile :drain}
         (legal-spells {:player {:mana 250}
                        :boss {:hp 100}
                        :effects [[:poison 3] [:shield 3] [:recharge 3]]})))
  (is (empty?
       (legal-spells {:player {:mana 0}
                      :boss {:hp 100}
                      :effects [[:recharge 3]]})))
  (is (= #{:magic-missile :drain}
         (legal-spells {:player {:mana 100}
                        :boss {:hp 100}
                        :effects [[:recharge 3]]})))
  (is (= #{:magic-missile :drain :shield :poison}
         (legal-spells {:player {:mana 200}
                        :boss {:hp 100}
                        :effects [[:recharge 1]]})))
  (is (= #{:magic-missile :drain :shield}
         (legal-spells {:player {:mana 200}
                        :boss {:hp 100}
                        :effects [[:recharge 1] [:poison 2]]}))))

(deftest search-test
  (is (= [226 [:poison :magic-missile]]
         (search {:player {:hp 10 :mana 250}
                  :boss   {:hp 13 :damage 8}})))

  (is (= [641 [:recharge :shield :drain :poison :magic-missile]]
         (search {:player {:hp 10 :mana 250}
                  :boss   {:hp 14 :damage 8}})))
  (is (= [1824
          [:recharge :poison :shield :recharge :poison :shield :recharge :poison :shield :magic-missile :poison :magic-missile]]
         (search {:player {:hp 50 :mana 500}
                  :boss   {:hp 71 :damage 10}}))))
