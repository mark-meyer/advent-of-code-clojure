(ns src.2017.16
  (:require [clojure.string :as str]))

(defn spin [s]
  (let [n (parse-long s)]
    (fn [programs]
      (into (subvec programs (- (count programs) n)) (subvec programs 0 (- (count programs) n))))))

(defn exchange [s]
  (let [[a b] (map parse-long (str/split s #"/"))]
    (fn [programs]
      (assoc programs a (programs b)  b (programs a)))))

(defn partner [s]
  (let [[c1 _ c2] s]
    (fn [programs]
      (let [a (.indexOf programs c1)
            b (.indexOf programs c2)]
        (assoc programs a (programs  b)  b (programs a))))))

(defn parse-instuctions [code]
  (let [move-type (first code)]
    (case move-type
      \s (spin (subs code 1))
      \x (exchange (subs code 1))
      \p (partner (subs code 1)))))


(def dance (->>
            (str/split (slurp "data/2017/16.txt") #",")
            (map parse-instuctions)
            (reverse)
            (apply comp)))

(def programs (into [] "abcdefghijklmnop"))

(def part-one (str/join (dance programs)))

part-one

;; a little bit of sluething:
;; shows there are only 36 patterns
(count (into #{} (take 100 (iterate dance programs))))

;; and the cylce starts at the begging
(last (take 37 (iterate dance programs)))

(str/join (last (take (mod 1000000000 36) (drop 1 (iterate dance programs)))))

