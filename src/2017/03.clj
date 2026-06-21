(ns src.2017.03)

(def N  (parse-long (slurp "data/2017/03.txt")))

;; Doing the math. Probably won't work in part two
(defn part-one [n]
  (let [sqrt (int (Math/ceil (Math/sqrt n)))
        corner (if (odd? sqrt) sqrt (inc sqrt))
        max-val (* corner corner)
        len (dec corner)
        mid (quot len 2)
        small-corner (first (for [c (range max-val 0 (- len)) :when  (< c n)] c))
        ] 
      (+ mid (Math/abs (- mid (- n small-corner))))))


(def neighbor-offsets 
  (for [dx (range -1 2)
        dy (range -1 2)
        :when (not= [dx dy] [0 0])]
    [dx dy]))


(defn neighbor-sum [grid [x y]]
  (apply + (map (fn [[dx dy]] (get grid [(+ dx x) (+ dy y)] 0)) 
         neighbor-offsets)))

(def spiral-coords
  (let [dirs  (cycle [[1 0] [0 1] [-1 0] [0 -1]])
        steps (mapcat #(repeat 2 %) (iterate inc 1))]
    (reductions (fn [[x y] [dx dy]] [(+ x dx) (+ y dy)])
                [0 0]
                (mapcat repeat steps dirs))))

(defn part-two [target]
  (->> (rest spiral-coords)
       (reductions (fn [[grid _] coord]
                     (let [v (neighbor-sum grid coord)]
                       [(assoc grid coord v) v]))
                   [{[0 0] 1} 1])
       (map second)
       (drop-while #(<= % target))
       first))

(part-one N)
(part-two N)

