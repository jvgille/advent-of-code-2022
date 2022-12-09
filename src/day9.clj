(ns day9 
  (:require 
   [clojure.string :as string]
   [clojure.test :refer [run-tests]]
   [defpure :refer [defpure]]))

(def ^:private example-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def ^:private example-input-2 "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(defpure parse-input
  {[example-input] ["R" "R" "R" "R"
                    "U" "U" "U" "U"
                    "L" "L" "L"
                    "D"
                    "R" "R" "R" "R"
                    "D"
                    "L" "L" "L" "L" "L"
                    "R" "R"]}
  [s]
  (->>
   s
   (string/split-lines)
   (map #(string/split %1 #" "))
   (map (fn [[d n]] (repeat (Integer/parseInt n) d)))
   (flatten)))

(defpure move-head
  {[[0 0] "U"] [ 0  1]
   [[0 0] "R"] [ 1  0]
   [[0 0] "D"] [ 0 -1]
   [[0 0] "L"] [-1  0]}
  [[x y] d]
  (case d
    "U" [x (+ y 1)]
    "D" [x (- y 1)]
    "R" [(+ x 1) y]
    "L" [(- x 1) y]))

;; Weird, different version of clojure or something?
(defn abs
  [x]
  (if (< x 0) (- x) x))

(defpure should-follow?
  {[[0 0] [0 0]] false ; head on tail
   [[1 0] [0 0]] false ; 1 square, same row
   [[0 1] [0 0]] false ; 1 square, same col
   [[2 0] [0 0]] true  ; 2 square, same row
   [[0 2] [0 0]] true  ; 2 square, same col
   [[2 1] [0 0]] true  ; 2 square
   [[1 2] [0 0]] true  ; 2 square
   [[2 2] [0 0]] true  ; 2 square diagonal
   }
  [[hx hy] [tx ty]]
  (< 1
     (max
      (abs (- hx tx))
      (abs (- hy ty)))))

(defn maybe-follow
  [[h0 h1] t]
  (if (should-follow? h1 t)
    h0 t))

(defpure move-rope
  {[[[0 0] [0 0]] "U"] [[0 1] [0 0]]
   [[[0 1] [0 0]] "R"] [[1 1] [0 0]]
   [[[1 1] [0 0]] "R"] [[2 1] [1 1]]
   [[[2 1] [1 1]] "D"] [[2 0] [1 1]]
   [[[2 0] [1 1]] "D"] [[2 -1] [2 0]]}
  "Move head in direction, tail follows."
  [[h t] d]
  (let [h1 (move-head h d)]
    [h1 (maybe-follow [h h1] t)]))

;; maybe follow is wrong for longer rope
;; no longer always follows in exactly same position
(defpure move-long-rope
  {[[[0 0] [0 0]] "U"] [[0 1] [0 0]]
   [[[0 1] [0 0]] "R"] [[1 1] [0 0]]
   [[[1 1] [0 0]] "R"] [[2 1] [1 1]]
   [[[2 1] [1 1]] "D"] [[2 0] [1 1]]
   [[[2 0] [1 1]] "D"] [[2 -1] [2 0]]
   [[[0 0] [1 0] [0 1] [1 2] [2 3]] "L"] [[-1 0] [0 0] [0 1] [1 2] [2 3]]
   }
  "Move head in direction. Rest of rope tail follows."
  [[h & ts] d]
  (->>
   ts
   (reductions (fn [p t] [t (maybe-follow p t)]) [h (move-head h d)])
   (mapv second)))

(defpure part1
  {[example-input] 13}
  [s]
  (->>
   s
   (parse-input)
   (reductions move-rope [[0 0] [0 0]])
   (map second)
   (set)
   (count)))

(defpure part2
  {[example-input] 1
   [example-input-2] 36
   }
  [s]
  (->>
   s
   (parse-input)
   (reductions move-long-rope (apply vector (repeat 10 [0 0])))
   (map last)
   (set)
   (count)))

(comment 
  (part2 example-input)
  (part1 (slurp "input/day9.txt"))
  )

(run-tests)
