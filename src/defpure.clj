;; Derived from
;; https://github.com/fredoverflow/clopad/blob/master/samples/defpure.clj

(ns defpure
  (:require [clojure.string :as string]
            [clojure.test :refer [do-report]]))

(defn- filter-stack-trace! [^Throwable throwable ns-name]
  (->>
   (. throwable getStackTrace)
   (filter #(string/starts-with? (. % getClassName) ns-name))
   (into-array StackTraceElement)
   (. throwable setStackTrace))
  throwable)

(defn register-test [v inputs->output-map do-report]
  ;; todo just filter on clojure/java etc (or just take first/last n lines?)
  (let [ns-name (name (ns-name (:ns (meta v))))]
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
                                    :actual   (filter-stack-trace! throwable ns-name)})))))))

(defmacro defpure
  "Defines a pure function, illustrated by an exemplary inputs->output map"
  [name inputs->output-map & rest]
  `(do
     (defn ~name ~@rest)
     (defpure/register-test (var ~name) ~inputs->output-map #(do-report %))))
