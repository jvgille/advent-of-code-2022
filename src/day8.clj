(ns day8
  (:require [clojure.string :as string]
            [clojure.test :refer [run-tests]]
            [defpure :refer [defpure]]
            [matrix :refer [transpose pairwise-or pairwise-product]]))

(def ^:private example-input
  "30373
25512
65332
33549
35390")

(defpure parse-input 
  {[example-input] [[3 0 3 7 3]
                    [2 5 5 1 2]
                    [6 5 3 3 2]
                    [3 3 5 4 9]
                    [3 5 3 9 0]]}
  "Transform input into matrix of numbers."
  [s]
  (->>
   s
   (string/split-lines)
   (map (fn [line] (map #(Character/getNumericValue %) line)))))

(defpure visible
  {[[3 0 3 7 3]] [true false false true false]
   [[2 5 5 1 2]] [true true false false false]
   [[6 5 3 3 2]] [true false false false false]
   [[3 3 5 4 9]] [true false true false true]
   [[3 5 3 9 0]] [true true false true false]}
  "Determines which trees are visible along a given line."
  [trees]
  (into [true] 
        (mapv #(apply < %) 
              (partition 2 1 
                         (reductions #(max %1 %2) trees)))))

(defpure visible-trees
  {[(parse-input example-input)] 
   [[true  true  true  true true]
    [true  true  true false true]
    [true  true false  true true]
    [true false  true false true]
    [true  true  true  true true]]}
  "Given matrix of trees of separate height, return a boolean matrix 
   of whether a tree is visible from outside the grid,"
  [m]
  (let [rl (map reverse m)
        td (transpose m)
        dt (map reverse td)
        lrv (map visible m)
        rlv (map visible rl)
        tdv (map visible td)
        dtv (map visible dt)]
    (pairwise-or
     lrv
     (map reverse rlv)
     (transpose tdv)
     (transpose (map reverse dtv)))))

(defpure part1
  {[example-input] 21}
  [s]
  (->>
   s
   (parse-input)
   (visible-trees)
   (flatten)
   (filter identity)
   (count)))

(defpure view-distance
  {[[3 0 3 7 3]] [0 1 2 3 1]
   [[2 5 5 1 2]] [0 1 1 1 2]
   [[6 5 3 3 2]] [0 1 1 1 1]
   [[3 3 5 4 9]] [0 1 2 1 4]
   [[3 5 3 9 0]] [0 1 1 3 1]} 
  "Given a list of trees, gives a list of how far each tree can see towards the
   beginning of the list."
  [trees]
  (loop [result []
         heightmap (apply vector (repeat 10 0))
         [tree & rest] trees]
    (if tree
      (let [num-visible (heightmap tree)]
        (recur
         (conj result num-visible)
         (into [] (map-indexed (fn [i v] (if (<= i tree) 1 (+ 1 v))) heightmap))
         rest))
      result)))

(defpure part2
  {[example-input] 8}
  [s]
  (->>
   (let [m (parse-input s)
         t (transpose m)]
     [(map view-distance m)
      (transpose (map view-distance t))
      (map reverse (map view-distance (map reverse m)))
      (transpose (map reverse (map view-distance (map reverse t))))])
   (apply pairwise-product)
   (flatten)
   (apply max)))

(comment
  (time 
   (part1 (slurp "input/day8.txt")))
  (time 
   (part2 (slurp "input/day8.txt")))
  )

(run-tests)
