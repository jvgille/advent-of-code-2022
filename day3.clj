(ns day3
  (:require
   [clojure.set]
   [common :refer [lines]]))

(defn first-half
  [s]
  (subs s 0 (/ (count s) 2)))

(defn second-half
  [s] 
  (subs s (/ (count s) 2)))

(defn misplaced-items 
  [contents] 
  (filter (set (first-half contents)) (second-half contents)))

(defn priority-of
  [item]
  (let [i (int item)] 
    (if (> i 95)
      (- i 96)
      (+ (- i 64) 26))))

(defn part1
  [filename]
  (->>
   filename
   (lines)
   (map misplaced-items)
   (map first)
   (map priority-of)
   (reduce +)))

(defn part2
  [filename]
  (->>
   filename
   (lines)
   (partition 3)
   (map #(apply clojure.set/intersection (map set %1)))
   (map first)
   (map priority-of)
   (reduce +)))

(comment
  (part1 "examples/day3.txt")
  (part2 "examples/day3.txt")
  (part1 "input/day3.txt")
  (part2 "input/day3.txt")
  )
