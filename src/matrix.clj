(ns matrix 
  (:require [defpure :refer [defpure]]
            [clojure.test :refer [run-tests]]))

(defn transpose
  [m]
  (apply map list m))

(defn transposev
  [m]
  (apply mapv vector m))

(defpure pairwise
  {
   [(fn [& v] (or (some identity v) false))
    [[true false false]
     [false false false]]
    [[false false false]
     [false true false]]
    [[false false false]
     [true false false]]] [[true false false]
                           [true true false]]
   [*
    [[4 2 1]
     [1 1 4]]
    [[9 1 9]
     [3 0 1]]
    [[0 1 0]
     [1 1 1]]] [[0 2 0]
                [3 0 4]]
   [+
    [[4 2 1]
     [1 1 4]]
    [[9 1 9]
     [3 0 1]]
    [[0 1 0]
     [1 1 1]]] [[13 4 10]
                [ 5 2  6]]
   }
  "Takes a variadic function and applies to corresponding elements
   across the given matrices."
  [op & ms]
  (->>
   (transpose ms)
   (mapv #(apply mapv op %))))

(defn pairwise-or 
  "Does pairwise or of given boolean matrices."
  [& ms]
  (apply pairwise (fn [& v] (or (some identity v) false)) ms))

(defn pairwise-product 
  "Does pairwise multiplication of given numeric matrices."
  [& ms]
  (apply pairwise * ms))

(comment 
  (run-tests)
  )
