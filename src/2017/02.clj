(ns src.2017.02
   (:require
    [clojure.string :as str]
    [clojure.test :refer [is]]))

(defn- parse-line
  "Conver a string of whitespace separated numbers to a sequence of numbers"
  [line]
  (->> (str/split line #"\s+")
       (map parse-long)))

(def data (->>
           (slurp "data/2017/02.txt")
           str/split-lines
           (map parse-line)
           ))

(defn checksum
  "Find the difference betweent the largest and smalled values in data"
  [data]
  (- (apply max data) (apply min data))
  )

(defn part-one 
  {:test
   (fn []
     (is (= 18 (part-one [[5 1 9 5]
                          [7 5 3]
                          [2 4 6 8]]))))}
  [data]
  (->> data
       (map checksum)
       (reduce +))
  )


(defn divisible
  "Find the only each pair that is evenly divisible and return result of division"
  [data]
  (let [sorted (sort data)]
    (first (for [tail (iterate rest sorted)
                 :while (seq tail)
                 :let [d (first tail)]
                 q  (rest tail)
                 :when (zero? (mod q d))] 
             (/ q d)))))

(defn part-two
  {:test (fn []
           (is (= 9 (part-two [[5 9 2 8]
                               [9 4 7 3]
                               [3 8 6 5]]))))}
  [data]
  (->> data 
       (map divisible)
       (reduce +)))

(part-one data)
(part-two data)
