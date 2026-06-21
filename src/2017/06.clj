(ns src.2017.06
  (:require [clojure.string :as str])
  (:require [clojure.test :refer [is]]))


(def data (->> (slurp "data/2017/06.txt")
               (#(str/split % #"\s"))
               (mapv parse-long)))

(defn arg-max 
  "returns the index of the largest item returns the first in the case of a tie"
  {:test (fn []
           (is (= 1 (arg-max [9 10 8 7 6])))
           (is (= 1 (arg-max [9 10 8 10 6])))
           (is (= 4 (arg-max [9 10 8 10 16]))))}
  [coll]
  (->> coll
       (map-indexed vector)
       (reduce (fn [winner curr]
                 (if (> (second curr) (second winner))
                   curr
                   winner)))
       first))


(defn part-one 
  "Redistribute blocks how many turns we arrive at a previously seen state"
  {:test (fn []
           (is (= 5 (part-one [0 2 7 0]))))}
  [data]
  (loop [data data
         seen #{}
         steps 0]
    (if (or (seen data) (> steps 50000))
      steps
      (let [max-index (arg-max data)
            length (count data)
            start-index (inc max-index)
            blocks (get data max-index)
            seen (conj seen data)]
        (recur (reduce
                (fn [current, index] (update current (mod index length) inc))
                (assoc data max-index 0)
                (range start-index (+ blocks start-index)))
               seen
               (inc steps))))))


(part-one data)

(defn part-two 
  "Redistribute blocks how many turns between identical state"
  {:test (fn []
           (is (= 4 (part-two [0 2 7 0]))))}
  [data]
  (loop [data data
         seen {}
         steps 0]
    (if (seen data)
      (- steps (seen data))
      (let [max-index (arg-max data)
            length (count data)
            start-index (inc max-index)
            blocks (get data max-index)
            seen (assoc seen data steps)]
        (recur (reduce
                (fn [current, index] (update current (mod index length) inc))
                (assoc data max-index 0)
                (range start-index (+ blocks start-index)))
               seen
               (inc steps))))))


(part-two data)
