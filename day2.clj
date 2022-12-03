(ns day2
  (:require
   [clojure.core :refer [slurp]]
   [clojure.string :refer [split]]))

(def score-of
  {"A X" (+ 3 1)
   "A Y" (+ 6 2)
   "A Z" (+ 0 3)
   "B X" (+ 0 1)
   "B Y" (+ 3 2)
   "B Z" (+ 6 3)
   "C X" (+ 6 1)
   "C Y" (+ 0 2)
   "C Z" (+ 3 3)})

(def produce-result
  {"A X" "A Z"
   "A Y" "A X"
   "A Z" "A Y"
   "B X" "B X"
   "B Y" "B Y"
   "B Z" "B Z"
   "C X" "C Y"
   "C Y" "C Z"
   "C Z" "C X"})

(defn part1
  [filename]
  (as-> filename n
    (slurp n)
    (split n #"\n")
    (map score-of n)
    (reduce + n)))

(defn part2
  [filename]
  (as-> filename n
    (slurp n)
    (split n #"\n")
    (map produce-result n)
    (map score-of n)
    (reduce + n)))

(comment
  (part1 "examples/day2.txt")
  (part2 "examples/day2.txt")
  (part1 "input/day2.txt")
  (part2 "input/day2.txt")
  )
