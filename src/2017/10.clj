(ns src.2017.10
  (:require
   [clojure.string :as str]
   [clojure.test :refer [is]]))

(def data (slurp "data/2017/10.txt"))

(defn data-int [data]
  (map parse-long (str/split data #",")))

(defn data-ascii [data]
  (concat (map int data) [17 31 73 47 23]))


(defn splice
  "Extract a sequence of len values starting at start-idx
   reverse them and put them back in, wrapping around if needed"
  {:test (fn []
           (is (= [1 4 3 2 5 6] (splice [1 2 3 4 5 6] 1 3)))
           (is (= [6 5 3 4 2 1] (splice [1 2 3 4 5 6] 4 4))))}
  [v start-idx len]
  (let [indices (map #(mod % (count v)) (range start-idx (+ start-idx len)))
        values (reverse (map v indices))
        pairs (map vector indices values)]
    (reduce (fn [vec [src val]] (assoc vec src val)) v pairs)))


(defn single-pass
  "Run a pass of splicing with each length in data"
  {:test (fn []
           (is (= [3 4 2 1 0] (first (single-pass [[0 1 2 3 4] [3, 4, 1, 5] 0 0])))))}
  [[vec data skip start-idx]]
  (reduce (fn [[vec data skip idx] len]
            [(splice vec idx len) data (inc skip) (+ idx len skip)])
          [vec data skip start-idx] data))


(defn part-one
  "do a single pass and mutliply the first tow results"
  {:test (fn []
           (is (= 12 (part-one [0 1 2 3 4] [3, 4, 1, 5]))))}
  [vec data]
  (let [skip 0
        start-idx 0
        [final_v] (single-pass [vec data skip start-idx])]
    (* (first final_v) (second final_v))))


(part-one (into [] (range 256)) (data-int data))

(defn part-two
  "Create the knot hash from the string"
  {:test (fn []
           (is (= "a2582a3a0e66e6e86e3812dcb672a272"  (part-two 256 "")))
           (is (= "33efeb34ea91902bb2f59c9920caa6cd"  (part-two 256 "AoC 2017")))
           (is (= "3efbe78a8d82f29979031a4aa0b16a9d"  (part-two 256 "1,2,3")))
           (is (= "63960835bcdc130f0b66d7ff4f6a5a8e"  (part-two 256 "1,2,4"))))} 

  [size data]
  (let [vec (into [] (range size))
        data (data-ascii data)
        [sparse] (nth (iterate single-pass [vec data 0 0]) 64)]
    (->> sparse
         (partition 16)
         (map #(apply bit-xor %))
         (map #(format "%02x" %))
         str/join)))

(part-two 256 data)

