(ns day12
  (:require [clojure.string :as string]
            [clojure.test :refer [run-tests]]
            [common :refer [find-indexed]]
            [defpure :refer [defpure]]
            [matrix]
            [graph]))

(def ^:private example 
"Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
")

(defpure find-char
  {[example \S] [0 0]
   [example \E] [2 5]}
  [s c]
  (let [result (->>
                (string/split-lines s)
                (map #(apply list %))
                (map (fn [row] (find-indexed #(= % c) row)))
                (find-indexed identity))]
    (and result [(first result) (first (second result))])))

(defn to-height
  [v]
  (case v
    \E 25
    \S 0
    (- (int v) 97)))

(defn heightmap
  [s]
  (mapv #(mapv to-height %) (string/split-lines s)))

(defn traversable?
  [from to]
  (<= (- to from) 1))

(defn connects?
  [heightmap p0 p1]
  (let [h0 (get-in heightmap p0)
        h1 (get-in heightmap p1)]
    (traversable? h0 h1)))

(defn part1
  [s]
  (let [start (find-char s \S)
        end (find-char s \E)
        heightmap (heightmap s)
        width (count (first heightmap))
        height (count heightmap)
        neighbors (->>
                   (for [x (range width) y (range height)] [y x])
                   (map (fn [p]
                          [p (->>
                              [[0 1] [1 0] [0 -1] [-1 0]]
                              (map #(mapv + p %))
                              (filter (fn [[y x]] (and (< -1 x width) (< -1 y height))))
                              (filter #(connects? heightmap p %)))]))
                   (into {}))]
    (graph/bfs start end neighbors)))

(defn part2
  "'Cheating' :P"
  [s]
  (let [end (find-char s \E)
        heightmap (heightmap s)
        width (count (first heightmap))
        height (count heightmap)
        neighbors (->>
                   (for [x (range width) y (range height)] [y x])
                   (map (fn [p]
                          [p (->>
                              [[0 1] [1 0] [0 -1] [-1 0]]
                              (map #(mapv + p %))
                              (filter (fn [[y x]] (and (< -1 x width) (< -1 y height))))
                              (filter #(connects? heightmap p %)))]))
                   (into {}))]
    (graph/bfs [29 0] end neighbors)))

(comment
  (time (part1 example))
  (def result (part1 example))
  (def result (part1 (slurp "input/day12.txt")))
  (def result (part2 (slurp "input/day12.txt")))
  
  (- (count result) 1)
  
  (->>
   result
   (reduce (fn [m p] (assoc-in m p \.))
           (heightmap (slurp "input/day12.txt")))
   (matrix/to-string #(case % 
                        \. " ."
                        (format "%2d" %)))
   (println)
   )
  )

(run-tests)
