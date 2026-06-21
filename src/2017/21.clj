(ns src.2017.21
  (:require [clojure.string :as str]))

(defn rot90 [pat]
  (mapv vec (apply map vector (reverse pat))))

(defn flip-h [m]
  (mapv #(vec (reverse %)) m))

(defn rotations [m]
  (take 4 (iterate rot90 m)))

(defn transforms [m]
  (distinct
   (concat
    (rotations m)
    (map flip-h (rotations m)))))

(defn flatten-key [m]
  (apply str (flatten m)))

(defn canonical-key [m]
  (str/join (map (fn [c] (if (= c \#) 1 0))
                 (first (sort (map flatten-key (transforms m)))))))

(defn rule-to-cannonical [rule-key]
  (canonical-key (mapv vec rule-key)))

(defn parse-pattern [pat]
  (let [[rule-input rule-output] pat]
    [(rule-to-cannonical (str/split rule-input #"/")) (str/split rule-output #"/")]))


(def data (->> (slurp "data/2017/21.txt")
               (str/split-lines)
               (map #(str/split % #" => "))

               (map parse-pattern)
               (into {})))


(defn subsize [pat]
  (let [size (count (first pat))]
    (cond
      (zero? (mod size 2)) 2
      (zero? (mod size 3)) 3)))

(defn combined-size [pat]
  (let [size (count pat)]
    (cond
      (= size 1) 1
      (zero? (mod size 2)) 2
      (zero? (mod size 3)) 3)))


(defn partition-image
  "Take an image and split it into smaller parts"
  [image]
  ;
  (if (= (count image) 1) image
      (let [size (subsize image)]
        (for [row-group (partition size image)
              col-group (apply map vector (map #(partition size %) row-group))]
          (->> col-group
               (map str/join))))))


(defn combine-image
  "Take seperated sections and stich them back to gether"
  [patterns]
  (let [size (combined-size patterns)]
    (->> patterns
         (partition size)
         (mapcat #(apply map vector %))
         (map #(apply str %)))))


(defn step
  "Take an entire pattern return an entire patter"
  [start]
  (->> start
       (partition-image)
       (map rule-to-cannonical)
       (map data)
       (combine-image)))


(defn part-one [start]
  (let [final (last (take 6 (iterate step [start])))]
    (count (filter #(= \# %) (apply str final)))))

(def start [".#." "..#" "###"])

(part-one start)