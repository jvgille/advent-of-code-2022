(ns day4
  (:require
   [clojure.string :refer [split]]))

(defn range-contained?
  "Determine if the range [a, b] fully contains the range [x, y]."
  [a b x y]
  (<= a x y b))

(defn inside?
  [a b x]
  (<= a x b))

(defn ranges-overlap?
  "Determine if the range [a, b] overlaps the range [x, y]."
  [a b x y]
  (or
   (inside? a b x)
   (inside? a b y)
   (inside? x y a)
   (inside? x y b)))

(defn parse-input
  [filename]
  (as-> filename n
    (slurp n)
    (split n #"[^\d]")
    (map #(Integer/parseInt %1) n)
    (partition 4 n)))

(defn part1
  [filename]
  (->> 
   filename
   (parse-input)
   (filter (fn [[a b c d]] 
             (or
              (range-contained? a b c d) 
              (range-contained? c d a b)))) 
   (count)))

(defn part2
  [filename]
  (->>
   filename
   (parse-input)
   (filter (fn [[a b c d]] (ranges-overlap? a b c d)))
   (count)))

(comment
  (part1 "examples/day4.txt")
  (part2 "examples/day4.txt")
  (part1 "input/day4.txt")
  (part2 "input/day4.txt")
  )
