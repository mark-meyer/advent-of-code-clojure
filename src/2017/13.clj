(ns src.2017.13
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (map parse-long (#(str/split line #": "))))

(def data (->> (slurp "data/2017/13.txt")
               (str/split-lines)
               (map parse-line)))

(defn cycle_length [depth] (* 2 (dec depth )))

(defn caught? [delay [range depth]] 
  (zero? (mod (+ delay range) (cycle_length depth))) )


(defn part-one [data start] 
  (reduce (fn [caught, [range depth]]
          (if (caught? start [range depth]) 
            (+ (* range depth) caught) 
            caught))
          0 data))


(part-one data 0)

(defn part-two [data]
  (->> (range)
       (filter (fn [start] (not-any? #(caught? start %) data)))
       (first)))

(part-two data)