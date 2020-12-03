(ns aoc-2020.day2
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]))

(def toy-data "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(defn parse-line [l]
  (let [[min-chars max-chars c password] (rest (re-find #"(\d+)-(\d+) ([a-zA-Z]): ([a-zA-Z]+)" l))]
    {:min (Integer/parseInt min-chars)
     :max (Integer/parseInt max-chars)
     :c (first c)
     :password password}))

(defn parse [s]
  (map parse-line
       (str/split-lines s)))

(defn valid-password? [m]
  (let [{:keys [min max c password]} m
        char-count (get (frequencies password)
                        c
                        0)]
    (<= min
       char-count
       max)))

(def data (slurp "resources/aoc_2020/day_2_1.txt"))


(let [d data]
  (count (filter valid-password? (parse d))))
;; => 422


(defn valid-password-v2? [m]
  (let [{:keys [min max c password]} m
        x (nth password (dec min))
        y (nth password (dec max))]
    (and (not= x y)
         (or (= c x)
             (= c y)))))

(let [d data]
  (count (filter valid-password-v2? (parse d))))
;; => 451

(deftest part-1
  (is (= 422 (let [d data]
               (count (filter valid-password? (parse d)))))))

(run-tests)
