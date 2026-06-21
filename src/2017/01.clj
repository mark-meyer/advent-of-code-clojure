(ns src.2017.01
  (:require 
   [clojure.string :as str]
   [clojure.test :refer [is]]
   ))

(def data (->>
           (slurp "data/2017/01.txt")
           str/trim
           (map #(Character/digit % 10)) 
           ))

(defn- solution 
  "Sum of digits if the digit equals the digit offest spaces away"
  [data offset]
  (let [n (count data)]
    (->> (cycle data)
         (drop offset)
         (map vector (cycle data))
         (take n)
         (filter (fn [[a b]] (= a b)))
         (map first)
         (reduce +))))
  

(defn part-one 
  "Compute sum of numbers that equal the following number "
  {:test (fn []
           (is (= 3 (part-one [1 1 2 2])))
           (is (= 4 (part-one [1 1 1 1])))
           (is (= 0 (part-one [1 2 3 4])))
           (is (= 9 (part-one [9 1 2 1 2 1 2 9]))))}
  [data] 
  (solution data 1))


(defn part-two
  "Compute sum of numbers that equal the number half the list length in front "
  {:test (fn []
           (is (= 6 (part-two [1 2 1 2])))
           (is (= 0 (part-two [1 2 2 1])))
           (is (= 4 (part-two [1 2 3 4 3 5])))
           (is (= 12 (part-two [1 2 3 1 2 3])))
           (is (= 4 (part-two [1 2 1 3 1 4 1 5]))))}
  [data]
  (solution data (/ (count data) 2)))

(part-one  data)
(part-two  data)
