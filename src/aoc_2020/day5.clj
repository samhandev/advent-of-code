(ns aoc-2020.day5
  (:require [aoc-2020.day5 :refer [sample-data]]
            [aoc-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]))

(def sample-data "FBFBBFFRLR
BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL")

(defn parse-line [l]
  (let [[_ row-s col-s] (re-find #"([F|B]{7})([L|R]{3})" l)]
    [row-s col-s]))

(defn parse [s]
  (map parse-line (str/split-lines s)))

(defn binary->dec [binary-str]
  (-> binary-str
      (str/replace #"F|B|R|L" {"B" "1" "F" "0"
                               "R" "1" "L" "0"})
      (Integer/parseInt 2)) )

(defn ->row-col [[row-s col-s]]
  (map binary->dec [row-s col-s]))

(defn ->seat-num [[row col]]
  (+ (* row 8) col))

(defn seat-numbers [data]
  (->> data
       parse
       (map ->row-col)
       (map ->seat-num)))

(seat-numbers sample-data)

(deftest sample-test-seats
  (is (= [357 567 119 820]
         (seat-numbers sample-data))))

(def data (utils/load-data 2020 5))

(defn max-seat [d]
  (->> d
       (seat-numbers)
       (apply max)))

(deftest part-1-max-seat
  (is (= 976
         (max-seat data))))


(let [sorted-seats (sort (seat-numbers data))]
  (first
   (drop-while (fn [[x y]]
                 (= (inc x) y))
               (map (fn [x y] [x y])
                    sorted-seats
                    (rest sorted-seats)))))

(defn missing-seat [d]
  (let [sorted-seats (sort (seat-numbers d))]
    (->> (map (fn [x y] [x y])
              sorted-seats
              (iterate inc (first sorted-seats)))
         (drop-while (fn [[x y]] (= x y)))
         first ;; first time they didn't match
         second))) ;; second is the expected seat number

(missing-seat data)

(deftest part-2-missing-seat
  (is (= 685 (missing-seat data))))

(run-tests)
