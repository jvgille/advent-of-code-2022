(ns day10 
  (:require [clojure.string :as string]
            [clojure.test :refer [run-tests]]
            [defpure :refer [defpure]]
            [common :refer [abs]]))

(def ^:private example-input (slurp "examples/day10.txt"))

(defn parse-program 
  [s]
  (->>
   (string/split-lines s)
   (map #(if (string/starts-with? % "addx")
           [:noop (Integer/parseInt (second (string/split % #" ")))]
           :noop))
   (flatten)
   (reductions (fn [x i] (if (= i :noop) x (+ x i))) 1)
   (into [1])))

(defpure part1
  {[example-input] 13140}
  "Find the signal strength during the 20th, 60th, 100th, 140th, 180th,
   and 220th cycles. What is the sum of these six signal strengths?"
  [s]
  (let [xs
        (->>
         (parse-program s)
         (map-indexed *))]
    (->>
     (map #(nth xs %) [20 60 100 140 180 220])
     (reduce +))))

(defpure draw
  {[0 1] "#"
   [1 3] "."
   [2 1] "#"
   [3 1] "."
   [4 4] "#"}
  "CRT is rendering position i; sprite is at position x."
  [i x]
  (if (> (abs (- i x)) 1)
    "."
    "#"))

(defpure part2
  {[example-input] "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######....."}
  "Render the image given by your program. What eight
   capital letters appear on your CRT?"
  [s]
  (->>
   (parse-program s)
   (drop 1)
   (partition 40)
   (map (partial map-indexed draw))
   (map string/join)
   (string/join "\n")))

(comment 
  (part1 example-input)
  (part1 (slurp "input/day10.txt"))
  
  (println (part2 example-input))
  (println (part2 (slurp "input/day10.txt")))
  )

(run-tests)
