(ns src.2017.23
  (:require [clojure.string :as str]))

(defn parse-arg
  "Returns k if it is a literal number otherwise returns the value in register k"
  [state y]
  (if-let [n (parse-long y)] n (get state  y 0)))

(defn math-f
  "Helper function to make an updating math function, Will update register x 
   by applying the function f with y
   "
  [x y f]
  (fn [state]
    (let [n (parse-arg state y)]
      (-> state
          (update x (fnil f 0) n)
          (update :counter inc)))))

(defn mul [x y]
 (fn [state]
  (let [n (parse-arg state y)]
        (-> state
        (update x (fnil * 0) n)
        (update :counter inc)
        (update :mul-count inc)
  ))))

(defn set-register [x y]
  (fn [state]
    (-> state
        (assoc x (parse-arg state y))
        (update :counter inc))))

(defn jnz [x y]
  (fn [state]
    (if (not= 0 (parse-arg state x))
      (update state :counter + (parse-arg state y))
      (update state :counter inc))))


(defn run-to-pause [state program] 
  (loop [state state]
    (let [state (update state :loop-count inc)]
      (if (contains? program (:counter state))
        (recur ((program (:counter state)) state))
        state))))


(defn parse-instuction [line]
  (let [[inst reg param] (str/split line #"\s")]
    (case inst
      "set" (set-register reg param)
      "add" (math-f reg param +)
      "sub" (math-f reg param -)
      "mul" (mul reg param)
      "jnz" (jnz reg param))))

(def program (->> (slurp "data/2017/23.txt")
                  (str/split-lines)
                  (map parse-instuction)
                  vec))


(def res (run-to-pause {:counter 0 :mul-count 0 :loop-count 0} program))

; part 1
(:mul-count res)

(defn composite? [n]
  (loop [d 2]
    (cond
      (zero? (mod n d)) true
      (> (* d d) n) false
      :else (recur (inc d)))))

(defn part-two [start]
  (let [b (- (* 100 (start "b")) -100000)
        c (inc (- b -17000))]
    (count (filter composite? (range b c 17)))
    ))

; Part two
(part-two res)