(ns common 
  (:require [clojure.string :refer [split-lines]]
            [clojure.test :refer [is]]))

;; todo - how to use with list? lazyseq, cons, persistentlist
(defn is=
  [actual expected]
  (is (= actual expected)))

(defn lines
  "Reads given file and splits on newlines."
  [f]
  (split-lines (slurp f)))

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
