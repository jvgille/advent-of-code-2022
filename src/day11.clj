(ns day11
  (:require [clojure.test :refer [run-tests]]
            [defpure :refer [defpure]]))

(def example-input "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
")

(defpure parse-operations
  {[example-input] 
   [["old" (resolve (symbol "*")) 19]
    ["old" (resolve (symbol "+")) 6]
    ["old" (resolve (symbol "*")) "old"]
    ["old" (resolve (symbol "+")) 3]]}
  [s]
  (->> 
   (re-seq #"Operation: new = (\w+) (.) (\w+)" s)
   (mapv #(drop 1 %))
   (mapv (fn [[lhs op rhs]]
           (let [parse #(if (= % "old") % (Integer/parseInt %))]
             [(parse lhs) (resolve (read-string op)) (parse rhs)])))))

(defpure parse-starting-items
  {[example-input] [[79 98] [54 65 75 74] [79 60 97] [74]]}
  [s] 
  (->>
   (re-seq #"Starting items: .*" s)
   (mapv (fn [line]
           (mapv
            #(Integer/parseInt %)
            (re-seq #"\d+" line))))))

(defpure parse-divisible-by
  {[example-input] [23 19 13 17]}
  [s]
  (mapv (comp #(Integer/parseInt %) second)
        (re-seq #"Test: divisible by (\d+)" s)))

(defpure parse-true-throw
  {[example-input] [2 2 1 0]}
  [s]
  (mapv (comp #(Integer/parseInt %) second)
        (re-seq #"If true: throw to monkey (\d+)" s)))

(defpure parse-false-throw
  {[example-input] [3 0 3 1]}
  [s]
  (mapv (comp #(Integer/parseInt %) second)
        (re-seq #"If false: throw to monkey (\d+)" s)))

(defpure evaluate-operation
  {[5 [7 * 3]] 21
   [5 [7 + 3]] 10
   [5 ["old" * 3]] 15
   [5 ["old" + 3]] 8
   [5 [3 * "old"]] 15
   [5 [3 + "old"]] 8
   [7 ["old" * "old"]] 49
   [7 ["old" + "old"]] 14}
  [old [lhs op rhs]]
  (let [substitute #(if (= % "old") old %)]
    (op (substitute lhs) (substitute rhs))))


;; (defpure do-round
;;   {[]}
;;   [operation divisible-by true-throw false-throw current-items]
;;   )


(comment
  (parse-operations example-input)
  (parse-starting-items example-input)
  (parse-divisible-by example-input)
  (parse-true-throw example-input)
  (parse-false-throw example-input)

  )

(run-tests)
