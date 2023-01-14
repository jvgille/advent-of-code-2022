(ns day14 
  (:require [clojure.string :as string]
            [clojure.test :refer [run-tests]]
            [defpure :refer [defpure]]
            [matrix]))


(def ^:private example-input 
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
")

(defpure parse-input
  {[example-input] [[[498 4] [498 6] [496 6]] 
                    [[503 4] [502 4] [502 9] [494 9]]]}
  [s]
  (->>
   (string/split-lines s)
   (map (fn [line]
          (->>
           (string/split line #"( -> |,)")
           (map #(Integer/parseInt %))
           (partition 2)
           (map #(into [] %)))))))

(defpure get-bounds
  {[(parse-input example-input)] [[494 503] [4 9]]}
  [lines]
   (let [coords (apply concat lines)
         xs (map first coords)
         ys (map second coords)
         bounds #(vector (apply min %) (apply max %))]
    [(bounds xs)
     (bounds ys)]))



(comment 
  (slurp "input/day14.txt")
  
  (matrix/create 2 2 nil)
  
  (let [lines (parse-input example-input)
        [[min-x max-x] [min-y max-y]] (get-bounds lines)
        width (- max-x min-x)
        height (- max-y min-y)
        lines (map (fn [line]
                     (map (fn [point] (update point 0  #(- % min-x)))
                          line))
                   lines)]
    (matrix/create width height nil)
    )
  
  (->> 
   example-input 
   (parse-input)
   (get-bounds)
   )
)

(run-tests)
