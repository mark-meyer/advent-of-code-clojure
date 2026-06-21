(ns src.2017.04
  (:require
   [clojure.string :as str]))

(def data (->>
           (slurp "data/2017/04.txt")
           str/split-lines
           (map #(str/split % #"\s+"))))

(defn- solve [data isvalid]
  (->> data
       (filter isvalid)
       count))

(defn valid-passphrase-1? [passphase]
  (apply distinct? passphase))
  

(defn part-one [data]
  (solve data valid-passphrase-1?))


(defn valid-passphrase-2? [passphase]
  (apply distinct? (map sort passphase)))
  

(defn part-two [data]
  (solve data valid-passphrase-2?))

(part-one data)
(part-two data)
