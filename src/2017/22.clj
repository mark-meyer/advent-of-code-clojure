(ns src.2017.22
    (:require [clojure.string :as str]))


(def directions ;; [row col]
  {:right {[-1 0] [0 1]
           [0 1] [1 0]
           [1 0] [0 -1]
           [0 -1] [-1 0]}
   :left {[-1 0] [0 -1]
          [0 -1] [1 0]
          [1 0] [0 1]
          [0 1] [-1 0]}
   :reverse {[-1 0] [1 0]
             [0 -1] [0 1]
             [1 0] [-1 0]
             [0 1] [0 -1]}
   :straight {[-1 0] [-1 0]
              [0 -1] [0 -1]
              [1 0] [1 0]
              [0 1] [0 1]}})

(def grid (->> (slurp "data/2017/22.txt")
                   (str/split-lines)))

(def start
  [(int (/ (count grid) 2))
   (int (/ (count (first grid)) 2))])


(def infected (set (for 
               [row (range (count grid))
                col (range (count (first grid)))
                :when (= \# (get-in grid [row col]))
                ] [row col])))


(defn move [[r c] [dr dc]]
  [(+ r dr) (+ c dc)])

(defn burst [state]
  (let [turn-dir (if ((state :infected) (state :pos)) :right :left)
        new-dir (get-in directions [turn-dir (state :dir)])
        new-count (if ((state :infected) (state :pos)) (state :count) (inc (state :count)))
        new-infected (if ((state :infected) (state :pos))
                       (disj (state :infected) (state :pos))
                       (conj (state :infected) (state :pos)))]
    {:pos (move (state :pos) new-dir)
     :dir new-dir
     :infected new-infected
     :count new-count}))

(def starting-state
  {:pos start :dir [-1 0] :infected infected :count 0})

; part one
((last (take 10001 (iterate burst starting-state))) :count)

;;; Part Two

(def virus-states 
  (into {} (for [pos infected] [pos :infected]))
  )

(defn next-virus-state [pos virus-states] 
  (case (virus-states pos)
    :flagged (dissoc virus-states pos)
    :weakened (assoc virus-states pos :infected)
    :infected (assoc virus-states pos :flagged) 
    (assoc virus-states pos :weakened)
))

(defn turn-dir [pos virus-states]
  (case (virus-states pos)
    :flagged :reverse
    :weakened :straight
    :infected :right
    :left))

(defn burst-2 [state]
  (let [turn-dir (turn-dir (state :pos) (state :virus-states))
        new-dir (get-in directions [turn-dir (state :dir)])
        new-count (if (= :weakened ((state :virus-states) (state :pos))) 
                    (inc (state :count)) 
                    (state :count))
        new-infected (next-virus-state (state :pos) (state :virus-states))]
    {:pos (move (state :pos) new-dir)
     :dir new-dir
     :virus-states new-infected
     :count new-count}))

(def starting-state-2
  {:pos start :dir [-1 0] :virus-states virus-states :count 0})

; part two
(:count (nth (iterate burst-2 starting-state-2) 10000000))

