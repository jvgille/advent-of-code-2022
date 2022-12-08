(ns day8
  (:require [clojure.string :as string]
            [clojure.test :refer [run-tests]]
            [common :refer [defpure transpose]]))

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
  {[[3 0 3 7 3]] '(true false false true false)
   [[2 5 5 1 2]] '(true true false false false)
   [[6 5 3 3 2]] '(true false false false false)
   [[3 3 5 4 9]] '(true false true false true)
   [[3 5 3 9 0]] '(true true false true false)}
  "Determines which trees are visible along a given line."
  [trees]
  (conj (map #(apply < %) (partition 2 1 (reductions #(max %1 %2) trees)))
        true))

(defpure matrix-or
  {[[[true false false]
     [false false false]]
    [[false false false]
     [false true false]]
    [[false false false]
     [true false false]]] [[true false false]
                           [true true false]]}
  "Does pairwise or of given boolean matrices."
  [& ms]
  (map (fn [v] (map #(or (some identity %) false) (transpose v))) (transpose ms)))

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
  (let [lr m
        rl (map reverse m)
        td (transpose m)
        dt (map reverse (transpose m))
        lrv (map visible lr)
        rlv (map visible rl)
        tdv (map visible td)
        dtv (map visible dt)]
    (matrix-or
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

(comment
  (part1 (slurp "input/day8.txt"))
  
  (def m [[1 2 3] [4 5 6] [7 8 9]])
  (map visible (parse-input example-input))
  (transpose (map visible (transpose (parse-input example-input))))

  m
  (transpose m)
  (map reverse m)
  (map reverse (transpose m))

  m
  (transpose (transpose m))
  (map reverse (map reverse m))
  (transpose (map reverse (map reverse (transpose m))))
  )

(run-tests)
