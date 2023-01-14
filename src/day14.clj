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

(defpure render-cave
  {[(parse-input example-input)]
   [[[0 0 0 0 0 0 0 0 0 0 0]
     [0 0 0 0 0 0 0 0 0 1 0]
     [0 0 0 0 0 0 0 0 0 1 0]
     [0 0 0 0 0 0 1 0 0 1 0]
     [0 0 0 0 0 0 1 0 0 1 0]
     [0 0 0 0 1 1 1 0 0 1 0]
     [0 0 0 0 0 0 0 0 0 1 0]
     [0 0 0 0 0 0 0 0 0 1 0]
     [0 0 0 0 0 0 0 0 0 1 0]
     [0 0 0 0 1 1 1 1 1 1 0]
     [0 0 0 0 1 0 0 0 0 0 0]
     [0 0 0 0 0 0 0 0 0 0 0]]
    7]}
  "Return a minimal matrix for the cave and the x coordinate sand will be dropped at."
  [lines]
  (let [[[min-x max-x] [_ max-y]] (get-bounds lines)
        width (+ (- max-x min-x) 3)
        height (+ max-y 2)
        lines
        (->>
         (map
          (fn [line]
            (->>
             (map (fn [point]
                    (update point 0 #(- (inc %) min-x)))
                  line)
             (partition 2 1)))
          lines)
         (apply concat))]
    [(reduce
      (fn [m [p0 p1]]
        (matrix/fill p0 p1 1 m))
      (matrix/create width height 0)
      lines)
     (- 501 min-x)]))

(comment 
  (slurp "input/day14.txt")
  
  (->>
   example-input
   (parse-input)
   (render-cave)
   (matrix/transpose)
   (matrix/to-string {0 \. 1 \# \+ \+})
   (println)
   )
  )

(run-tests)
