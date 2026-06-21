(ns src.2017.18
  (:require [clojure.string :as str])
  (:import [clojure.lang PersistentQueue]))

(defn parse-arg 
  "Returns k if it is a literal number otherwise returns the value in register k"
  [state k]
  (if-let [n (parse-long k)] n (get state  k 0)))

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

(defn snd [x]
  (fn [state]
    (-> state 
        (update :counter inc) 
        (assoc :last-played (get state x 0)))))

(defn set-register [x y]
  (fn [state]
    (-> state
        (assoc x (parse-arg state y))
        (update :counter inc))))

(defn rcv [x] 
  (fn [state] 
    (let [state (update state :counter inc)]
      (if (pos? (get state x 0))
        (assoc state :received  (get state :last-played))
        state))))            

(defn jgz [x y] 
  (fn [state]
    (if (pos? (parse-arg state x))
      (update state :counter + (parse-arg state y)) 
      (update state :counter inc))))


(defn parse-instuction [line]
  (let [[inst reg param] (str/split line #"\s")]
    (case inst
      "snd" (snd reg)
      "set" (set-register reg param)
      "add" (math-f reg param +)
      "mul" (math-f reg param *)
      "mod" (math-f reg param mod)
      "rcv" (rcv reg)
      "jgz" (jgz reg param))))

(def program (->> (slurp "data/2017/18.txt")
                  (str/split-lines)
                  (map parse-instuction)
                  vec))


(defn next-state [[state program]]
  (let [instr (get program (:counter state))]
    [(instr state) program]))


(:received (first (drop-while 
        (fn [state] (nil? (:received state))) 
        (map first (take 2000 (iterate next-state [ {:counter 0} program]))))))



;;
;; Part Two
;;

(defn rcv-2
  "If :queue has a value pop it and store it in
   register x. If queue is empty, wait and set :dead to true"
  [x]
  (fn [state]
    (if-let [value (first (:queue state))]
      (-> state
          (assoc x value)
          (assoc :waiting false)
          (update :queue pop) 
          (update :counter inc))
      (assoc state :waiting true))))

(defn snd-2 
  "'sends' the value of x and increase sent
   Sending means this will add values to its
   outbox to deliver on each turn
   "
  [x] 
  (fn [state]
     (-> state
               (update :sent inc)
               (update :counter inc)
               (update :outbox conj (parse-arg state x)))))


(defn parse-instuction-2 [line]
  (let [[inst reg param] (str/split line #"\s")]
    (case inst
      "snd" (snd-2 reg)
      "set" (set-register reg param)
      "add" (math-f reg param +)
      "mul" (math-f reg param *)
      "mod" (math-f reg param mod)
      "rcv" (rcv-2 reg)
      "jgz" (jgz reg param))))


(def program2 (->> (slurp "data/2017/18.txt")
                  (str/split-lines)
                  (map parse-instuction-2)
                  vec))

(defn make-state [p]
  {:sent 0
   "p" p
   :waiting false
   :queue PersistentQueue/EMPTY
   :outbox []
   :counter 0})

(defn out-of-bounds [state program]
  (or (< (:counter state) 0) (>= (:counter state) (count program))))

(defn run-to-pause [state program]
  (loop [state state]
    (if (or
         (out-of-bounds state program)
         (true? (:waiting state)))
      state
      (recur ((program (:counter state)) state)))))
      
  
(run-to-pause (make-state 1) program2)

(defn finished? [state program]
  (or (:waiting state) (out-of-bounds state program))
  )

(defn part-2 [state0 state1 program]
  (loop [state0 state0 state1 state1] 
    (if (and (finished? state0 program) (finished? state1 program))
      state1
      (let [state0 (run-to-pause state0 program)
            state1 (run-to-pause state1 program)]
        (recur
         (-> state0
             (update :queue into(:outbox state1))
             (assoc :waiting (and (empty?  (:outbox state1)) (:waiting state0)))
             (assoc :outbox []))
         (-> state1
             (update :queue into(:outbox state0))
             (assoc :waiting (and (empty? (:outbox state0)) (:waiting state1)))
             (assoc :outbox [])))))))

(:sent (part-2 (make-state 0) (make-state 1) program2))

