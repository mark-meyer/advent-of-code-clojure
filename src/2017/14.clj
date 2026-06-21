(ns src.2017.14
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test :refer [is]]))

(def key-string (slurp "data/2017/14.txt"))

(defn data-ascii [data]
  (concat (map int data) [17 31 73 47 23]))

(def bin-lookup {\0 [0 0 0 0]
                 \1 [0 0 0 1]
                 \2 [0 0 1 0]
                 \3 [0 0 1 1]
                 \4 [0 1 0 0]
                 \5 [0 1 0 1]
                 \6 [0 1 1 0]
                 \7 [0 1 1 1]
                 \8 [1 0 0 0]
                 \9 [1 0 0 1]
                 \a [1 0 1 0]
                 \b [1 0 1 1]
                 \c [1 1 0 0]
                 \d [1 1 0 1]
                 \e [1 1 1 0]
                 \f [1 1 1 1]})

(defn to-bin [hash]
   (into [] (mapcat bin-lookup hash)))

(defn rev-splice 
    "Extract a sequence of len values starting at start-idx
     reverse them and put them back in, wrapping around if needed"
  {:test (fn []
           (is (= [1 4 3 2 5 6] (rev-splice [1 2 3 4 5 6] 1 3)))
           (is (= [6 5 3 4 2 1] (rev-splice [1 2 3 4 5 6] 4 4))))}
  [v start-idx len]
  (let [size (count v)]
    (loop [curr-v v i 0]
      (if (>= i (quot len 2))
        curr-v
        (let [idx1 (mod (+ start-idx i) size)
              idx2 (mod (+ start-idx len -1 (- i)) size)]
          (recur (assoc curr-v
                        idx1 (curr-v idx2)
                        idx2 (curr-v idx1))
                 (inc i)))))))

(defn single-pass
  "Run a pass of splicing with each length in data"
  {:test (fn []
           (is (= [3 4 2 1 0] (first (single-pass [[0 1 2 3 4] [3, 4, 1, 5] 0 0])))))}
  [[vec data skip start-idx]]

  (reduce (fn [[vec data skip idx] len]
            [(rev-splice vec idx len) data (inc skip) (+ idx len skip)])
          [vec data skip start-idx]
          data))

(defn knot-hash
  "Create the knot hash from the string"
  {:test (fn []
           (is (= "a2582a3a0e66e6e86e3812dcb672a272"  (knot-hash "")))
           (is (= "33efeb34ea91902bb2f59c9920caa6cd"  (knot-hash "AoC 2017")))
           (is (= "3efbe78a8d82f29979031a4aa0b16a9d"  (knot-hash "1,2,3")))
           (is (= "63960835bcdc130f0b66d7ff4f6a5a8e"  (knot-hash "1,2,4"))))}

  [data]
  (let [vec (into [] (range 256))
        data (data-ascii data)
        [sparse] (nth (iterate single-pass [vec data 0 0]) 64)]
    (->> sparse
         (partition 16)
         (map #(apply bit-xor %))
         (map #(format "%02x" %))
         str/join)))

(defn make-binary-grid [key-string]
  (->> (range 128)
       (map #(str/join [key-string "-" %]))
       (map knot-hash)
       (map to-bin)
       vec))

(memoize
 (defn grid [key-string] (make-binary-grid key-string)))


(defn part-one [key-string]
  (let [grid (grid key-string)]
    (reduce + (flatten grid))))
     
(part-one key-string)

(defn grid->active-coordinate [grid]
  (set (for [x (range 128)
             y (range 128)
             :when (= 1 (get-in grid [x y]))]
         [x y])))

(defn neighbors [[x y]]
  [[x (dec y)] [x (inc y)]
   [(inc x) y] [(dec x) y]])


(defn get-connected [active-coords start]
  (loop [seen #{}
         stack [start]]
    (if-let [current (peek stack)]
      (if (seen current)
        (recur seen (pop stack))
        (recur
         (conj seen current)
         (into (pop stack) 
               (filter active-coords (neighbors current)))))
      seen)))


(defn part-two [key-string] 
  (let [grid (grid key-string)] 
    (loop [active-coords (grid->active-coordinate grid)
           regions 0]
      (if (empty? active-coords)
        regions
        (let [region (get-connected active-coords (first active-coords))]
          (recur (set/difference active-coords region) (inc regions)))))))
  
  
(part-two key-string)
