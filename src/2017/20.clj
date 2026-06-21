(ns src.2017.20
  (:require [clojure.string :as str]
            [clojure.set :as cset]))

(defn make-particle [line]
  (let [[pos vel acc] (->> line
                           (re-seq #"-?\d+")
                           (map parse-long)
                           (partition 3))]
    {:position pos :velocity vel :acceleration acc}))


(def data (->> (slurp "data/2017/20.txt")
               (str/split-lines)
               (map make-particle)))

(defn manhattan-magnitude [v]
  (->> v
       (map Math/abs)
       (reduce +)))


; part
(def part-one (first (sort-by second (->> data
                                          (mapv :acceleration)
                                          (map manhattan-magnitude)
                                          (map-indexed vector)))))
         
part-one

(defn add-vector [v1 v2]
  (map + v1 v2))

(defn subtract-vector [v1 v2]
  (map - v1 v2))

(defn update-velocity [p]
  (update p :velocity add-vector (:acceleration p)))

(defn update-position [p]
  (update p :position add-vector (:velocity p)))


(defn next-postion
  "Handy for testing, but probably to slow to
   step though every state for every particle"
  [particle]
  (-> particle
      (update-velocity)
      (update-position)))

(defn perfect-square? [n]
  (let [r (Math/round (Math/sqrt n))]
    (= n (* r r))))

(defn divisible? [n d]
  (and (not (zero? d))
       (zero? (mod n d))))

(defn get-roots [p1 p2]
  (let [[dp dv da] (subtract-vector p1 p2)
        A da
        B (+ da (* 2 dv))
        C (* 2 dp)]
    (cond
      ;; Constant case: 0t² + 0t + C = 0
      (and (zero? A) (zero? B))
      (if (zero? C)
        :all
        [])

      ;; Linear case: Bt + C = 0
      (zero? A)
      (let [num (- C)
            den B]
        (if (divisible? num den)
          (let [t (/ num den)]
            (if (>= t 0)
              [t]
              []))
          []))

      ;; Quadratic case: At² + Bt + C = 0
      :else
      (let [D (- (* B B) (* 4 A C))]
        (if (or (neg? D)
                (not (perfect-square? D)))
          nil
          (let [s   (int (Math/sqrt D))
                den (* 2 A)
                nums [(+ (- B) s)
                      (- (- B) s)]
                roots (->> nums
                           (filter #(divisible? % den))
                           (map #(/ % den))
                           (filter #(>= % 0))
                           distinct
                           vec)]
            (when (seq roots)
              roots)))))))

(defn components [p] 
  [(:position p) (:velocity p) (:acceleration p)])

(defn intersects? [p1 p2]
  (let [roots (map get-roots
                   (apply map vector (components p1))
                   (apply map vector (components p2)))]

    (cond
      (some nil? roots) false
      :else (seq (apply cset/intersection (map set (remove #(= :all %) roots)))))))

        
(defn part-two [points]
  (reduce (fn [remaining p1]
            (if (contains? remaining p1)
              (let [intersected (filter #(and (not= p1 %) (intersects? % p1)) remaining)]
                (if (seq intersected)
                  (apply disj remaining p1 intersected)
                  remaining))
              remaining))
          (set points)
          points))
(count (part-two data))

(defn tick [points]
  (->> points
       (map next-postion)
       (group-by :position)
       (vals)
       (filter #(= 1 (count %)))
       (map first)
       ))

(count (nth (iterate tick data ) 200))