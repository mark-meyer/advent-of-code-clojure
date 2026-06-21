(ns src.2017.05
  (:require
   [clojure.string :as str]))

(def data
  (->> (slurp "data/2017/05.txt")
       (str/split-lines)
       (map Integer/parseInt)
       vec))

(defn transform-one [[data idx]]
  [(update data idx inc) (+ idx (get data idx))])

(defn transform-two [[data idx]]
  (let [jump-fn  (if (>= (get data idx) 3) dec inc)]
  [(update data idx jump-fn) (+ idx (get data idx))]))

(defn part-one [] 
  (count (take-while 
          (fn [[data offset]] (contains? data offset))
          (iterate transform-one [data 0]))))

(defn  part-two [] 
  (count (take-while
          (fn [[data offset]] (contains? data offset))
          (iterate transform-two [data 0]))))

(part-one)
(part-two)


;; can we speed up part two a bit?

(defn alt-part-two [data]
  (loop [data data
         idx 0
         steps 0]
    (if (contains? data idx) 
      (recur 
       (update data idx (if (>= (get data idx) 3) dec inc))
       (+ idx (get data idx))
       (inc steps))
      steps)))

(alt-part-two data)

; using transient is a bit quicker

(defn alt-part-two-trans [data]
  (loop [data (transient data) idx 0 steps 0]
    (if (contains? data idx)
      (let [jump (get data idx)
            jump-fn (if (>= jump 3) dec inc)]
        (recur (assoc! data idx (jump-fn jump))
               (+ idx jump)
               (inc steps)))
      steps)))

(alt-part-two-trans data)
