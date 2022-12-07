(ns day6
  (:require [clojure.core :refer [distinct?]]
            [common :refer [find-indexed]]))

(def examples 
  ["mjqjpqmgbljsphdztnvjfqwrcgsmlb" 7 19
   "bvwbjplbgvbhsrlpgdmjqwftvncz" 5 23
   "nppdvjthqldpwncqszvftbrmjlhg" 6 23
   "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 10 29
   "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 11 26])

(defn find-unique-substr
  [s len]
  (->>
   (partition len 1 s)
   (find-indexed #(apply distinct? %))
   (first)
   (+ len)))

(defn part1 [s] (find-unique-substr s 4))
(defn part2 [s] (find-unique-substr s 14))

(comment 
  (->> examples 
       (partition 3)
       (map (fn [[ex ans1 ans2]] [(part1 ex) ans1 (part2 ex) ans2]))) 
  (part1 (slurp "input/day6.txt"))
  (part2 (slurp "input/day6.txt"))
  )
