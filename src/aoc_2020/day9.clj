(ns aoc-2020.day9
  (:require [aoc-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def data (utils/load-data 2020 9))

(def sample-data "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(defn parse [s]
  (map edn/read-string (str/split-lines s)))

(defn preable-sum [d n-preamble n]
  (let [numbers (subvec d (- n n-preamble) n)]
    (for [x numbers
          y numbers]
      (+ x y))))

(defn part-1 [d n-preamble]
  (loop [n n-preamble
         preamble (preable-sum d n-preamble n)]
    (if (not (contains? (set preamble) (nth d n)))
      (nth d n)
      (recur (inc n) (preable-sum d n-preamble (inc n))))))

(part-1 (vec (parse data)) 25)

(time
 (part-1 (vec (parse data)) 25))

(defn sum-values [d start end]
  (apply + (take (- end start) (drop start d))))

(defn contiguous-sum [d target]
  (loop [start 0
         end 1
         current-sum (sum-values d start end)]
    (cond (= current-sum target) (subvec (vec d) start end)
          (< current-sum target) (recur start (inc end) (+ current-sum (nth d end)))
          (> current-sum target) (recur (inc start) end (- current-sum (nth d start))))))

(defn sum-min-max [v]
  (+ (apply min v)
     (apply max v)))

(defn part-2 [d target]
  (sum-min-max (contiguous-sum d target)))

(let [d (vec (parse data))]
  (part-2 d (part-1 d 25)))
