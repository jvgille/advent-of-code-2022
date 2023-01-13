(ns day13 
  (:require [clojure.string :as string]
            [clojure.test :refer [run-tests]]
            [defpure :refer [defpure]]))

(def example-input "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
")

(defpure compare-packets
  {[[1 1 3 1 1] [1 1 5 1 1]] true
   [[[1] [2 3 4]] [[1] 4]] true
   [[9] [[8 7 6]]] false
   [[[4 4] 4 4] [[4 4] 4 4 4]] true
   [[7 7 7 7] [7 7 7]] false
   [[] [3]] true
   [[[[]]] [[]]] false}
  [left right]
  (cond
    (and (integer? left) (integer? right))
    (cond
      (< left right) true
      (> left right) false
      :else :continue)

    (and (vector? left) (vector? right))
    (let [v (->>
             (map vector left right)
             (map (fn [[l r]] (compare-packets l r)))
             (filter #(not= :continue %))
             (first))]
      (cond
        (boolean? v) v
        (< (count left) (count right)) true
        (> (count left) (count right)) false
        :else :continue))

    (vector? left)
    (compare-packets left [right])

    :else
    (compare-packets [left] right)))

(defn- parse-input
  [s]
  (->>
   (string/split-lines s)
   (replace {"" "nil"})
   (map read-string)
   (partition 3)
   (map drop-last)))

(defpure part1 
  {[example-input] 13}
  [s]
  (->>
   (parse-input s)
   (map (fn [[a b]] (compare-packets a b)))
   (keep-indexed (fn [i v] (if v i)))
   (map #(+ 1 %))
   (reduce +)
   ))

(comment
  (part1 (slurp "input/day13.txt"))
  
  )

(run-tests)
