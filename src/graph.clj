(ns graph 
  (:require [clojure.test :refer [run-tests]]
            [defpure :refer [defpure]]))

(defpure build-path
  {[{4 0
     0 3
     3 1
     1 2} 4] '(2 1 3 0 4)
   [{0 2
     1 0
     2 1} 2] '(0 1 2)}
  "Traverses the parent map backwards from the goal and returns the
   resulting path, ignoring previously seen nodes."
  [parents goal]
  (loop [seen #{}
         path '()
         curr goal]
    (if (or (nil? curr) (seen curr))
      path
      (recur (conj seen curr)
             (conj path curr)
             (parents curr)))))

(defpure dfs-traverse
  {[0 #{3} {0 [0 1]
            1 [0 1 2]
            2 [0 1 2 3]
            3 []}]
   {1 0
    2 1
    3 2}

   [0 #{4} {0 [1 2]
            1 [0 3]
            2 [0 3]
            3 [1 2 4]}]
   {1 0
    2 0
    3 1
    4 3}

   [0 #{4} {0 [1 2]
            1 [2 3]
            2 [3 0]
            3 [0 1]}]
   {1 0
    2 0
    3 1}}
  [start is-goal? neighbors-of]
  (loop [queue (list start)
         visited #{}
         parents {}]
    (let [[current & queue] queue]
      (cond
        (nil? current) parents
        (is-goal? current) parents
        (visited current) (recur queue visited parents)
        :else (let [visited (conj visited current)
                    neighbors (filter (complement visited) (neighbors-of current))]
                (recur
                 (concat neighbors queue)
                 visited
                 (reduce #(assoc %1 %2 current) parents (filter (complement parents) neighbors))))))))

(defpure dfs
  {[0 #{3} {0 [0 1]
            1 [0 1 2]
            2 [0 1 2 3]
            3 []}] 
   [0 1 2 3]
   
   [0 #{3} {0 [0 1 2]
            1 [0 1 2]
            2 [0 1 2]
            3 [0 1 2]}] 
   nil
   
   [0 #{4} {0 [1 :shortcut]
            1 [2]
            2 [3]
            3 [4]
            4 []
            :shortcut [4]}] 
   [0 1 2 3 4]}
  [start is-goal? neighbors-of]
  ;; (let [result (dfs-traverse start is-goal? neighbors-of)]
  ;;   ())
  (loop [queue (list start)
         visited #{}
         parents {}]
    (let [[current & queue] queue]
      (cond
        (nil? current) nil
        (is-goal? current) (build-path parents current)
        (visited current) (recur queue visited parents)
        :else (let [visited (conj visited current)
                    neighbors (filter (complement visited) (neighbors-of current))]
                (recur
                 (concat neighbors queue)
                 visited
                 (reduce #(assoc %1 %2 current) parents (filter (complement parents) neighbors))))))))

(defpure bfs
  {[0 #{3} {0 [0 1]
            1 [0 1 2]
            2 [0 1 2 3]
            3 []}]
   [0 1 2 3]

   [0 #{3} {0 [0 1 2]
            1 [0 1 2]
            2 [0 1 2]
            3 [0 1 2]}]
   nil

   [0 #{4} {0 [:shortcut 1]
            1 [2]
            2 [3]
            3 [4]
            4 []
            :shortcut [4]}]
   [0 :shortcut 4]}
  [start is-goal? neighbors-of]
  (loop [queue (list start)
         visited #{}
         parents {}]
    (let [[current & queue] queue]
      (cond
        (nil? current) nil
        (is-goal? current) (build-path parents current)
        (visited current) (recur queue visited parents)
        :else (let [visited (conj visited current)
                    neighbors (filter (complement visited) (neighbors-of current))]
                (recur
                 (concat queue neighbors)
                 visited
                 (reduce #(assoc %1 %2 current) parents (filter (complement parents) neighbors))))))))


;; (defpure bfs-traverse
  ;; {[0 {0 [0 1]
  ;;      1 [0 1 2]
  ;;      2 [0 1 2 3]
  ;;      3 []}] {0 0
  ;;              1 0
  ;;              2 1
  ;;              3 2}
  ;;  [0 {0 [0 1 2]
  ;;      1 [0 1 2]
  ;;      2 [0 1 2]
  ;;      3 [0 1 2]}] {0 0
  ;;                   1 0
  ;;                   2 1
  ;;                   3 nil}
  ;;  [0 {0 [:shortcut 1]
  ;;      1 [2]
  ;;      2 [3]
  ;;      3 [4]
  ;;      4 []
  ;;      :shortcut [4]}] {0 nil
  ;;                       1 0
  ;;                       2 1
  ;;                       3 2
  ;;                       4 :shortcut
  ;;                       :shortcut 0}}
  ;; {}
  ;; [start neighbors-of])

(run-tests)
