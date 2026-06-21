(ns src.2017.25
  (:require [clojure.string :as str]))


(def raw (-> (slurp "data/2017/25.txt")
               (str/split #"\n\n")
               ))


(defn starting-state [[preamble _input]]
  (let [[_s start-state count] (first (re-seq #"(?s)Begin in state ([A-Z]).+?(\d+)" preamble))]
    (println "pre " preamble)
    {:state start-state :steps (parse-long count) :location 0 :tape #{}}))

(defn instructions [instr]
  (let [lines (str/split-lines instr)
        values (for [line lines] (last (str/split line #"\W")))
        [state-key & instructions] values 
        ]
    [state-key 
     (for [[_if-current write move next-state] (partition 4 instructions)]
       {:write (parse-long write)
        :move (if (= move "right") 1 -1)
        :next-state next-state})]))


(def state (starting-state raw))
(def program (into {} (for [inst (rest raw)] (instructions inst))))

(defn write-tape [tape location val]
  (if (zero? val)
    (disj tape location)
    (conj tape location))
  )

(defn step [state]
  (let [cur-val (if ((:tape state) (:location state)) 1 0)
         insts (program (:state state))
         cur-inst (if (zero? cur-val) (first insts) (second insts))]
    (-> state 
        (update :tape write-tape (:location state) (:write cur-inst))
        (update :location + (:move cur-inst))
        (update :steps dec)
        (assoc :state (:next-state cur-inst))
        )))
  
  
  (count (:tape (first (drop-while #(pos? (:steps %)) (iterate step state )))))
