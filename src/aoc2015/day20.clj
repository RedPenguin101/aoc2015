(ns aoc2015.day20)

"Day 20: Infinite Elves and Infinite Houses
 
 sends them down a street with infinite houses numbered sequentially: 1, 2, 3, 4, 5
 Each Elf is assigned a number, too
 The first Elf (number 1) delivers presents to every house: 1, 2, 3, 4, 5, ....
 The second Elf (number 2) delivers presents to every second house: 2, 4, 6, 8, 10, ....
 Each Elf delivers presents equal to ten times his or her number

 What is the lowest house number of the house to get at least as 
 many presents as the number in your puzzle input? (29000000)
 
 Concept inventory:
 House, Elf, Presents
 
 presents:: house-number -> number-of-presents
 
 which elves deliver to which house?
 
 for elf in range 1-house
    if house mod elf == 0, 10*elf"

(defn presents [house]
  (apply + (keep #(when (zero? (mod house %)) %) (range 1 (inc house)))))

(first (drop-while #(<= % 290000) (map presents (range))))