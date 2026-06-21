(ns src.2017.09
  (:require 
   [clojure.test :refer [is]]))
   
(def data (slurp "data/2017/09.txt"))

(defn get-current-score [in-garbage char score]
  (cond
    in-garbage score
    (= char \{) (inc score)
    (= char \}) (dec score)
    :else score))
    
(defn get-total-score [in-garbage char current-score total-score]
  (cond
    in-garbage total-score
    (= char \}) (+ current-score total-score)
    :else total-score))
  

(defn part-one 
  "Count nested braces and score by depth avoiding garbage"
  {:test (fn []
           (is (= 1 (part-one "{}")))
           (is (= 6 (part-one "{{{}}}")))
           (is (= 5 (part-one "{{},{}}")))
           (is (= 16 (part-one "{{{},{},{{}}}}")))
           (is (= 1 (part-one "{<a>,<a>,<a>,<a>}")))
           (is (= 9 (part-one "{{<ab>},{<ab>},{<ab>},{<ab>}}")))
           (is (= 9 (part-one "{{<!!>},{<!!>},{<!!>},{<!!>}}")))
           (is (= 3 (part-one "{{<a!>},{<a!>},{<a!>},{<ab>}}")))
           )}
  [line]
  (loop [line line
         total-score 0
         current-score 0
         in-garbage false]
    (if (seq line)
      (let [current (first line)
            rest-line (if (= current \!)
                        (drop 2 line)
                        (rest line))
            in-garbage (or (= current \<) (and in-garbage (not= current \>)))
            total-score (get-total-score in-garbage current current-score total-score)
            current-score (get-current-score in-garbage current current-score)]

        (recur rest-line total-score current-score in-garbage))
      total-score)))


(defn part-two 
  "Now count all the garbage in the text"
  {:test (fn []
           (is (= 0 (part-two "<>")))
           (is (= 17 (part-two "<random characters>")))
           (is (= 3 (part-two "<<<<>")))
           (is (= 2 (part-two "<{!>}>")))
           (is (= 0 (part-two "<!!>")))
           (is (= 0 (part-two "<!!!>>")))
           (is (= 10 (part-two "<{o\"i!a,<{i<a>"))))}
  [line]
  (loop [line line
         garbage-count 0
         in-garbage false]
    (if (seq line)
      (let [current (first line)
            rest-line (if (= current \!) 
                        (drop 2 line)
                        (rest line))
            garbage-count (if (and in-garbage (not= current \>) (not= current \!)) 
                            (inc garbage-count) 
                            garbage-count)
            in-garbage (or (= current \<)  (and in-garbage (not= current \>)))]
        
        (recur rest-line garbage-count in-garbage))
      garbage-count)))

(part-one data)
(part-two data)
