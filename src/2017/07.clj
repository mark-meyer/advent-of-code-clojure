(ns src.2017.07
  (:require [clojure.string :as str]
            [clojure.set]))

(defn parse_line [line]
  (let [[parent weight & edges] (re-seq #"[a-z0-9]+" line)]
    [parent, (Integer/parseInt weight) edges]))

(def data (->>
           (slurp "data/2017/07.txt")
           str/split-lines
           (map parse_line)))

(defn part-one [data]
  (let [sources (set (map #(get % 0) data))
        dests (set (mapcat #(get % 2) data))]
    (clojure.set/difference sources dests)))


(defn make-graph [data]
  (reduce
   (fn [graph [key, weight, children]] 
     (assoc graph key {:weight weight :children children})) 
   {} data))

(defn find-unique-by [f coll]
  (let [freqs (frequencies (map f coll))]
    (filter #(= 1 (freqs (f %))) coll)))


(def get-weight
  (memoize
   (fn [graph key]
     (let [node (get graph key)]
       (if (nil? (:children node))
         (:weight node)
         (reduce +  (:weight node) (map (partial get-weight graph) (:children node))))))))

(defn child-weights [graph key]
  (let [node (get graph key)
        child-weights (map (partial get-weight graph) (:children node))]
    (if (apply = child-weights)
      nil
      child-weights)))

(defn part-two [graph]
  (loop
   [current-key (first (part-one data))
    parent nil]
    (let [next-child (first (find-unique-by #(get-weight graph %) (:children (get graph current-key))))]
      (if (seq next-child)
        (recur next-child, current-key)
        (let [weights (child-weights graph parent)
              sorted   (sort-by val > (frequencies weights))
              common  (ffirst sorted)
              odd-one (first (second sorted))]
          (+ (:weight (get graph current-key)) (- common odd-one)))))))

(part-one data)
(part-two (make-graph data))