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

(defpure ordered?
  {[1 2] true
   [2 1] false
   [2 2] :continue
   [[3] [1 2]] true
   [[1 2] [3]] false
   [[1 2] [2 2]] true
   [[2 2] [1 2]] false
   [[1 2] [1 2]] :continue
   [3 [1 2]] true
   [[1 2] 3] false
   [1 [1]] :continue
   [[1] 1] :continue}
  [left right]
  (println "left: " left "right:" right)
  (cond
    (and (integer? left) (integer? right))
    (cond
      (< left right) true
      (> left right) false
      :else :continue)

    (and (vector? left) (vector? right))
    (cond
      (< (count left) (count right)) true
      (> (count left) (count right)) false
      :else
      (let [v (->>
               (map vector left right)
               (map (fn [[l r]] (ordered? l r))) ;; supposed to compare recursively before looking at sizes
               (filter #(not= :continue %))
               (first))]
        (if (nil? v) :continue v)))

    (vector? left)
    (ordered? left [right])

    :else
    (ordered? [left] right)))

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
  ;;  (map #(ordered? (get % 0) (get % 1)))
   
   )

  )

(comment
  (ordered? [1 2] [1 2])
  
  (part1 example-input)
  (ordered? [[1] [2 3 4]] [[1] 4])
  (parse-input example-input)
  
  ()
  (apply map vector [[1 2 3] [4 5 6]])
  (map vector [1 2] [3 4])
  
  (some #(not= :continue %) [:continue false :continue])
  
  (list? [])
  )

(run-tests)
