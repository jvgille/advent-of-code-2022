(ns day1
  (:require
   [common :refer [lines split-by]]))

(defn part1
  [filename]
  (->>
   filename
   (lines)
   (split-by #(= "" %1))
   (map #(map read-string %1))
   (map #(reduce + %1))
   (apply max)
   ))

(defn part2
  [filename]
  (as-> filename n
    (lines n)
    (split-by #(= "" %1) n) 
    (map #(map read-string %1) n)
    (map #(reduce + %1) n)
    (sort n)
    (take-last 3 n)
    (reduce + n)))
 
(comment
  (part1 "examples/day1.txt")
  (part2 "examples/day1.txt")
  (part1 "input/day1.txt")
  (part2 "input/day1.txt")
  )
