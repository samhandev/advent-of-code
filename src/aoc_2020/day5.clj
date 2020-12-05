(ns aoc-2020.day5
  (:require [clojure.string :as str]))

(def sample-data "FBFBBFFRLR
BFFFBBFRRR
BFFFBBFRRR
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

(def data (slurp "resources/aoc_2020/day_5.txt"))

(->> data
     (seat-numbers)
     (apply max))
;; => 976

(let [sorted-seats (sort (seat-numbers data))]
  (first
   (drop-while (fn [[x y]]
                 (= (inc x) y))
               (map (fn [x y] [x y])
                    sorted-seats
                    (rest sorted-seats)))))
;; => [684 686]
