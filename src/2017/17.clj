(ns src.2017.17)

(def N (parse-long (slurp "data/2017/17.txt")))

(defn insert-at [path index val]
  (let [head (subvec path 0 index)
        tail (subvec path index)]
    (-> head
        (conj val)
        (into tail))))

(defn move [[path index]]
  (let [new-index (inc (mod (+ N index) (count path)))
        new-path (insert-at path new-index (count path))]
    [new-path new-index]))

(defn part-one []
  (let [[last-path last-index] (last (take 2018 (iterate move [[0] 0])))]
    (last-path (inc last-index))))

(part-one)

(defn next-index [index, step]
  (inc (mod (+ N index) (inc step))))

(defn part-two []
  (first (reduce
          (fn [[cur-idx-1 idx] step]
            (let [next-idx (next-index idx step)]
              (if (= idx 1)
                [step next-idx]
                [cur-idx-1 next-idx])))
          [0 1]
          (range 1 50000001))))

(part-two)