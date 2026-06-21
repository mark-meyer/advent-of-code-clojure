(ns src.2017.11
  (:require [clojure.string :as str]))

(def data (str/split (slurp "data/2017/11.txt") #","))

(defn add-coord [c1 c2]
  (map + c1 c2))

(defn hex-dist
  "Distance in 3D hex coords is just half manahattan distance
   but since these coordinates sum to zero we can just take the
   max absolute coordinate"
  [coord]
  (apply max (map abs coord)))

(def direction-map
  {"n" [-1 0 1]
   "s" [1 0 -1]
   "ne" [-1 1 0]
   "sw" [1 -1 0]
   "se" [0 1 -1]
   "nw" [0 -1 1]})

(defn part-one
  "Use 3d coordinates to represent the three degrees of freedom
   The final distance is just manhattan distance / 2"
  [data]
  (->> data
       (map direction-map)
       (reduce add-coord)
       (hex-dist)))

(part-one data)

(defn part-two [data]
  (->> data
       (map direction-map)
       (reductions add-coord)
       (map hex-dist)
       (apply max)))

(part-two data)
