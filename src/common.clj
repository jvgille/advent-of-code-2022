(ns common 
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string]))

(defn lines
  "Reads given file and splits on newlines."
  [f]
  (string/split-lines (slurp f)))

(defn split-by
  "Partitions coll, splitting on items for which pred returns a truthy value.
   Items that are split on are not included in result.
   
   (split-by #(mod %1 3) '(1 2 3 4 5 6))
   
   ((1 2) (4 5) (7))"
  [pred coll]
  (->>
   coll
   (partition-by pred)
   (filter #(not (and (= (count %1) 1) (pred (first %1)))))))

(defn find-indexed
  "Returns [index, item] for the first item in the collection which satisfies the predicate,
   or nil if no item does."
  [pred coll]
  (first (keep-indexed #(if (pred %2) [%1 %2] nil) coll)))

(defn pprint-spit
  [filename content]
  (spit filename (with-out-str (pprint content))))

;; Weird, different version of clojure or something?
(defn abs
  [x]
  (if (< x 0) (- x) x))

(defn clamp
  [x a b]
  (min (max x a) b))

(defn chessboard-difference
  [a b]
  (apply max (mapv abs (mapv - a b))))
