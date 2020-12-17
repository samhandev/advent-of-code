(ns aoc-2020.day13
  (:require [aoc-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(def sample-data "939
7,13,x,x,59,x,31,19")

(def data (utils/load-data 2020 13))

(defn parse-time-table [s]
  (let [d (str/split-lines s)
        [depart-time bus-ids] d]
    {:depart-time (Integer/parseInt depart-time)
     :bus-ids (map #(Integer/parseInt %)
                   (filter #(not= "x" %) (str/split bus-ids #",")))}))

(def time-table (parse-time-table sample-data))

(defn bus-departures [bus-id]
  (iterate #(+ % bus-id) bus-id))

(defn next-departure-after [depart-time bus-id]
  (* (int (Math/ceil (/ depart-time bus-id)))
     bus-id))

(take 10 (bus-departures 7))

(next-departure-after 939 7)

(defn earliest-depart [{:keys [depart-time bus-ids]}]
  (map (fn [bus-id]
         [bus-id (next-departure-after depart-time bus-id)])
       bus-ids))

(let [time-table (parse-time-table sample-data)
      depart-time (:depart-time time-table)
      [bus-id bus-depart] (first
                           (sort-by second (earliest-depart time-table)))]
  (* bus-id (- bus-depart depart-time)))
;; => 295

(let [time-table (parse-time-table data)
      depart-time (:depart-time time-table)
      [bus-id bus-depart] (first
                           (sort-by second (earliest-depart time-table)))]
  (* bus-id (- bus-depart depart-time)))
;; => 119

;;part 2
;; 7,13,x,x,59,x,31,19
;; find a number such that
;; a * 7 = (b * 13) - 1 = (c * 59) - 4 = (d * 31) - 6 = (e * 19) - 7

(defn rem-bus-id [s]
  (let [d (second (str/split-lines s))]
    (keep-indexed (fn [i v] (when (not= "x" v)
                              (let [value (Integer/parseInt v)]
                                [i value])))
                  (str/split d #","))))


(defn bus-departures-with-offset [bus-id offset]
  (iterate #(+ % bus-id) offset))

(defn p2-depart [data]
  (let [d (rem-bus-id data)
        buses d]
    (loop [step (second (first buses))
           depart-time 0
           [[offset bus-id] & rest-bus] (rest buses)]
      (if (nil? bus-id)
        depart-time
        (recur (* step bus-id)
               (->> (bus-departures-with-offset step depart-time)
                    (filter (fn [x] (= (mod (+ offset x) bus-id)
                                       0)))
                    first)
               rest-bus)))))

(comment

  (let [d (rem-bus-id data)
        buses d]
    (loop [step (second (first buses))
           depart-time 0
           [[offset bus-id] & rest-bus] (rest buses)]
      (if (nil? bus-id)
        depart-time
        (recur (* step bus-id)
               (->> (bus-departures-with-offset step depart-time)
                    (filter (fn [x] (= (mod (+ offset x) bus-id)
                                       0)))
                    first)
               rest-bus))))
;; => 1106724616194525
  )
