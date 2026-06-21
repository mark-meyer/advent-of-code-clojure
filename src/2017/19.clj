(ns src.2017.19
  (:require [clojure.string :as str]))

(def grid (->> (slurp "data/2017/19.txt")
               (str/split-lines)))

(def char-lookup
  (into {}
        (for [[row line] (map-indexed vector grid)
              [col ch]   (map-indexed vector line)
              :when (not= \space ch)]
          [[row col] ch])))

(defn add-dir [pos dir]
  (mapv + dir pos))

(defn move [state] (-> state
  (update :position add-dir (:direction state))
  (update :steps inc)))

(defn turn [state]
  (let [pos (:position state)]
    (cond
      (zero? (first (:direction state))) (if (char-lookup (add-dir pos [1 0]))
                                           (assoc state :direction [1 0])
                                           (assoc state :direction [-1 0]))
      :else  (if (char-lookup (add-dir pos [0 1]))
               (assoc state :direction [0 1])
               (assoc state :direction [0 -1])))))

(defn next-position [state]
  (let [current-char (char-lookup (:position state))]
    (cond
      (Character/isLetter current-char)
      (-> state
          (update :letters conj current-char)
          (move))

      (= current-char \+)
      (-> state
          (turn)
          (move))

      :else (move state))))


(def state {:position [0 (.indexOf (grid 0) "|")]
            :direction [1 0]
            :steps 0
            :letters []})

(defn run-map [state]
         (first (drop-while (fn [state] (char-lookup (:position state)))
                            (iterate next-position state))))

(def final-state (run-map state))

;; part 1
(apply str (:letters final-state))

;; part 2
(:steps final-state)
