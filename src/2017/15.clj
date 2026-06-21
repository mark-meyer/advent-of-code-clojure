(ns src.2017.15)

(defn parse-iput []
  (->> (slurp "data/2017/15.txt")
       (re-seq #"(?s)\d+")
       (map parse-long)))

(def inp (parse-iput))
(def genA [(first inp) 16807])
(def genB [(second inp) 48271])
(def MASK 65535)

(defn generator [[n multiple]]
  (drop 1 (iterate #(mod (* % multiple) 2147483647) n)))

(defn last-bits-equal [a b]
  (if (= (bit-and a MASK) (bit-and b MASK)) 1 0))

(defn part-one [genA genB]
  (transduce (take 40000000)
   +
   0
   (map last-bits-equal (generator genA) (generator genB))))


(part-one genA genB)

(defn generator-two [[n multiple] divisor]
  (->> (iterate #(mod (* % multiple) 2147483647) n)
       (drop 1)
       (filter #(zero? (mod % divisor)))))

(defn part-two [genA genB]
  (apply + (take 5000000 (map last-bits-equal (generator-two genA 4) (generator-two genB 8)))))

(part-two genA genB)

