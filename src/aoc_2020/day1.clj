(ns aoc-2020.day1)

(def toy-data "1721
979
366
299
675
1456")

(def data (slurp "resources/aoc_2020/day_1_1.txt"))

(defn parse [s]
  (map (fn [x] (Integer/parseInt x))
       (clojure.string/split-lines s)))

(defn get-product [matches]
  (last (first matches)))

(def part-1-matches
  (let [inputs (parse data)]
    (for [x inputs
          y inputs
          :when (= (+ x y) 2020)]
      [x y (* x y)])))


(def part-2-matches
  (let [inputs (parse data)]
    (for [x inputs
          y inputs
          z inputs
          :when (= (+ x y z) 2020)]
      [x y z (* x y z)])))


(get-product part-1-matches)
;; => 326211


(get-product part-2-matches)
;; => 131347190

