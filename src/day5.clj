(ns day5
  (:require [clojure.string :refer [split-lines, split]]))

(def example-input
 "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn split-input
  [input]
  (split input #"\r?\n\r?\n"))
  
(comment
  (split-input example-input))
  
(defn initialize-stacks
  [input] 
  (apply
   vector
   (let [lines (->>
                input
                (split-lines)
                (map #(take-nth 4 (drop 1 %)))
                (reverse)
                (next))
         num-stacks (count (first lines))]
     (reduce
      (fn
        [stacks line]
        (map-indexed
         (fn [i stack]
           (let [item (nth line i)]
             (if
              (= item \space)
               stack
               (conj stack item))))
         stacks))
      (repeat num-stacks [])
      lines))))

(comment
  (initialize-stacks (first (split-input example-input)))
  (initialize-stacks (first (split-input (slurp "input/day5.txt")))) 
  )
  
(defn parse-instructions
  [input]
  (->>
   input
   (re-seq #"\d+")
   (map #(Integer/parseInt %))
   (partition 3)
   (map (fn [[n f t] ] [n (- f 1) (- t 1)])))) 

(comment 
  (parse-instructions (second (split-input example-input)))
  (parse-instructions (second (split-input (slurp "input/day5.txt")))) 
  )

(defn evaluate-instruction
  [stacks instruction should-reverse?]
  (let [[num-crates from-idx to-idx] instruction
        from-stack (stacks from-idx)
        to-stack (stacks to-idx)
        [removed-from crates] (split-at
                               (- (count from-stack) num-crates)
                               from-stack)
        added-to (concat to-stack (if should-reverse? (reverse crates) crates))]
    (assoc stacks from-idx removed-from to-idx added-to)))

(defn evaluate-instructions
  [input should-reverse?]
  (let [[stacks-input instructions-input] (split-input input)] 
    (loop [stacks (initialize-stacks stacks-input)
           instructions (parse-instructions instructions-input)] 
      (if (empty? instructions)
        stacks
        (recur
         (evaluate-instruction stacks (first instructions) should-reverse?)
         (next instructions))))))

(comment 
  (evaluate-instructions example-input true)
  )
  
(defn part1
  [input] 
  (clojure.string/join (map last (evaluate-instructions input true))))
  
(comment
  (part1 example-input)
  (part1 (slurp "input/day5.txt")))

(defn part2
  [input]
  (clojure.string/join (map last (evaluate-instructions input false))))

(comment
  (= (part2 example-input) "MCD")
  (= (part2 (slurp "input/day5.txt")) "FGLQJCMBD"))
