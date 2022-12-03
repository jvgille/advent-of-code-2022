(ns day1
  (:require
   [clojure.core :refer [slurp]]
   [clojure.string :refer [split]]))

(defn part1
  [filename]
  (as-> filename n
    (slurp n)
    (split n #"\n\n")
    (map #(split %1 #"\n") n)
    (map #(map read-string %1) n)
    (map #(reduce + %1) n)
    (apply max n)))

(defn part2
  [filename]
  (as-> filename n
    (slurp n)
    (split n #"\n\n")
    (map #(split %1 #"\n") n)
    (map #(map read-string %1) n)
    (map #(reduce + %1) n)
    (sort n)
    (take-last 3 n)
    (reduce + n)
    )) 

(comment
  (part1 "examples/day1.txt")
  (part2 "examples/day1.txt")
  (part1 "input/day1.txt")
  (part2 "input/day1.txt"))
