(ns graph 
  (:require [clojure.test :refer [run-tests]]
            [defpure :refer [defpure]]))

(defn- build-path
  "Traverses the parent HashMap backwards from the goal and returns the
   resulting path. Assumes the start of the path has no parent!"
  [parents goal]
  (loop [seen (new java.util.HashSet)
         path '()
         curr goal]
    (if (or (nil? curr) (. seen contains curr))
      path
      (do 
        (. seen add curr)
        (recur seen (conj path curr) (. parents get curr))))))

(defpure dfs
  {[0 3 {0 [0 1]
         1 [0 1 2]
         2 [0 1 2 3]
         3 []}] [0 1 2 3]
   [0 3 {0 [0 1 2]
         1 [0 1 2]
         2 [0 1 2]
         3 [0 1 2]}] nil
   [0 4 {0 [:shortcut 1]
         1 [2]
         2 [3]
         3 [4]
         4 []
         :shortcut [4]}] [0 1 2 3 4]}
  [start goal neighbors-of]
  (loop [queue (new java.util.LinkedList [start])
         visited (new java.util.HashSet #{start})
         parents (new java.util.HashMap)]
    (if-let [current (and (seq queue) (. queue removeFirst))]
      (if (= current goal)
        (build-path parents goal)
        (do
          (->>
           (neighbors-of current)
           (filter (complement #(. visited contains %)))
           (map (fn [next]
                  (. visited add next)
                  (. parents put next current)
                  (. queue addFirst next)))
           (doall))
          (recur queue visited parents)))
      nil)))

(defpure bfs
  {[0 3 {0 [0 1]
         1 [0 1 2]
         2 [0 1 2 3]
         3 []}] [0 1 2 3]
   [0 3 {0 [0 1 2]
         1 [0 1 2]
         2 [0 1 2]
         3 [0 1 2]}] nil
   [0 4 {0 [:shortcut 1]
         1 [2]
         2 [3]
         3 [4]
         4 []
         :shortcut [4]}] [0 :shortcut 4]}
  [start goal neighbors-of] 
  (loop [queue (new java.util.LinkedList [start])
         visited (new java.util.HashSet #{start})
         parents (new java.util.HashMap)]
    (if-let [current (and (seq queue) (. queue removeFirst))]
      (if (= current goal)
        (build-path parents goal)
        (do
          (->>
           (neighbors-of current)
           (filter (complement #(. visited contains %)))
           (map (fn [next]
                  (. visited add next)
                  (. parents put next current)
                  (. queue addLast next)))
           (doall))
          (recur queue visited parents)))
      nil)))


(defpure bfs-traverse
  {[0 {0 [0 1]
       1 [0 1 2]
       2 [0 1 2 3]
       3 []}] {0 0
               1 0
               2 1
               3 2}
   [0 {0 [0 1 2]
       1 [0 1 2]
       2 [0 1 2]
       3 [0 1 2]}] {0 0
                    1 0
                    2 1
                    3 nil}
   [0 {0 [:shortcut 1]
       1 [2]
       2 [3]
       3 [4]
       4 []
       :shortcut [4]}] {0 nil
                        1 0
                        2 1
                        3 2
                        4 :shortcut
                        :shortcut 0}}
  [start neighbors-of]
  
  )

(run-tests)
