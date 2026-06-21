(ns src.2017.12
  (:require
   [clojure.string :as str]))

(defn parse-line [line]
  (let [[node & edges] (re-seq #"\d+" line)]
    {(parse-long node) (map parse-long edges)}))

(def data (->>
           (slurp "data/2017/12.txt")
           (str/split-lines)
           (map parse-line)
           (apply merge)))

(defn get-connected [graph start]
  (loop [seen #{}
         stack [start]]
    (if-let [current (peek stack)]
      (if (seen current)
        (recur seen (pop stack))
        (recur
         (conj seen current)
         (into (pop stack)
               (remove seen (get graph current [])))))
       seen)))

(defn part-one [data] (count (get-connected data 0)))

(defn part-two [data]
  (first (reduce
          (fn [[count seen] k]
            (if (seen k)
              [count seen]
              [(inc count) (into seen (get-connected data k))])) [0 #{}] (keys data))))

(part-one data)
(part-two data)