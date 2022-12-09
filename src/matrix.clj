(ns matrix 
  (:require [defpure :refer [defpure]]))

(defn transpose
  [m]
  (apply map list m))

(defn transposev
  [m]
  (apply mapv vector m))

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

(defpure matrix-product
  {[[[4 2 1]
     [1 1 4]]
    [[9 1 9]
     [3 0 1]]
    [[0 1 0]
     [1 1 1]]] [[0 2 0]
                [3 0 4]]}
  "Does pairwise multiplication of given boolean matrices."
  [& ms]
  (map (fn [v] (map #(apply * %) (transpose v))) (transpose ms)))

