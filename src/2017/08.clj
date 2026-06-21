(ns src.2017.08
  (:require [clojure.string :as str]))

(defn make-function [f arg register]
  (case f
    "inc" (fn [registers] (update registers register (fnil + 0) arg)) 
    "dec" (fn [registers] (update registers register (fnil - 0) arg))))

(defn make-predicate [cmp register rhs]
  (let [f (case cmp
            ">"  >
            "<"  <
            "==" =
            ">=" >=
            "<=" <=
            "!=" not=)
        rhs-val (Integer/parseInt rhs)]
    (fn [registers] (f (get registers register 0) rhs-val ))))

(defn parse-line [line]
  (let [[register f arg _ lhs cmp rhs] (str/split line #"\s+")]
    {:f (make-function f (Integer/parseInt arg) register)
     :pred (make-predicate cmp lhs rhs)
     :register register}))

(def data (->>
           (slurp "data/2017/08.txt")
           str/split-lines
           (map parse-line)))

(defn part-one [data]
  (let [final-register
        (reduce
         (fn [registers {:keys [pred f]}]
           (if (pred registers)
             (f registers)
             registers))
         {} data)]
    (apply max (vals final-register))))

(defn part-two [data]
  (second (reduce
           (fn [[registers max-seen] {:keys [pred f register]}]
             (if (pred registers)
               (let [new-register (f registers)]
                 [new-register (max max-seen (get new-register register))])
               [registers max-seen]))
           [{} 0] data)))

(part-one data)
(part-two data)