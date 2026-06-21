(ns src.2017.24
  (:require [clojure.string :as str]))


(defn make-component [line]
  (let [comp (mapv parse-long (str/split line #"/"))
        weight (+ (first comp) (second comp))]
    [weight comp]))

(def data (->> (slurp "data/2017/24.txt")
               (str/split-lines)
               (map make-component)
               (set)))

(defn connect? [open [a b]]
  (or (= open a) (= open b)))

(defn open-port [open [a b]]
  (if (= open a) b a))

(defn available-pipes [components open]
  (filter (fn [[_weight, comp]] (connect? open comp)) components))

  
(def max-pipe
  (memoize
   (fn [components open] 
     (let [available (available-pipes components open)]
       (if (seq available)
         (reduce max (for [[w comp] available]
                       (+ w (max-pipe
                             (disj components [w comp])
                             (open-port open comp)))))
         0)))))

; part one
(max-pipe data 0)


(def max-pipe-len
  (memoize
   (fn [components open]
     (let [available (available-pipes components open)]
       (if (seq available)
         (reduce (fn [a b] (if (pos? (compare b a)) b a))
                 (for [[w comp] available]
                       (let [[l w2] (max-pipe-len
                             (disj components [w comp])
                             (open-port open comp))]
                         [(+ 1 l) (+ w w2)])))
         [0 0])))))

; part two
(max-pipe-len data 0)
