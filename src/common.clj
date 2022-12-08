(ns common 
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.test :refer [do-report]]))

(defn lines
  "Reads given file and splits on newlines."
  [f]
  (string/split-lines (slurp f)))

(defn transpose
  [m]
  (apply map list m))

(defn transposev
  [m]
  (apply mapv vector m))

(defn split-by
  "Partitions coll, splitting on items for which pred returns a truthy value.
   Items that are split on are not included in result.
   
   (split-by #(mod %1 3) '(1 2 3 4 5 6))
   
   ((1 2) (4 5) (7))"
  [pred coll]
  (->>
   coll
   (partition-by pred)
   (filter #(not (and (= (count %1) 1) (pred (first %1)))))))

(defn find-indexed
  "Returns [index, item] for the first item in the collection which satisfies the predicate,
   or nil if no item does."
  [pred coll]
  (first (keep-indexed #(if (pred %2) [%1 %2] nil) coll)))

(defn pprint-spit
  [filename content]
  (spit filename (with-out-str (pprint content))))

(defn- filter-stack-trace! [^Throwable throwable]
  (->>
   (. throwable getStackTrace)
   (filter (complement
            #(let [className (. % getClassName)]
               (or
                (string/starts-with? className "clojure")
                (string/starts-with? className "java")
                (string/starts-with? className "nrepl")))))
   (into-array StackTraceElement)
   (. throwable setStackTrace))
  throwable)

;; register-test and defpure from
;; https://github.com/fredoverflow/clopad/blob/master/samples/defpure.clj
(defn register-test [v inputs->output-map do-report]
  (alter-meta! v assoc :test
               #(doseq [[inputs output] inputs->output-map]
                  (try
                    (let [actual (apply @v inputs)]
                      (do-report {:type     (if (= output actual) :pass :fail)
                                  :message  (str "  inputs: " inputs)
                                  :expected output
                                  :actual   actual}))
                    (catch Throwable throwable
                      (do-report {:type     :error
                                  :message  (str "  inputs: " inputs)
                                  :expected output
                                  :actual   (filter-stack-trace! throwable)}))))))

(defmacro defpure
  "Defines a pure function, illustrated by an exemplary inputs->output map"
  [name inputs->output-map & rest]
  `(do
     (defn ~name ~@rest)
     (common/register-test (var ~name) ~inputs->output-map #(do-report %))))
