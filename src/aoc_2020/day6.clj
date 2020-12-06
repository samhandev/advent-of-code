(ns aoc-2020.day6
  (:require [aoc-2020.utils :as utils]
            [clojure.string :as str]))

(def data (utils/load-data 2020 6))

(def sample-data "abc

a
b
c

ab
ac

a
a
a
a

b")

(let [s sample-data]
  (->> (str/split s #"\n\n")
       (map #(frequencies %))
       (map #(dissoc % \newline))
       (map #(count %))))
;; => (3 3 3 1 1)


(let [s data]
  (->> (str/split s #"\n\n")
       (map #(frequencies %))
       (map #(dissoc % \newline))
       (map #(count %))
       (apply +)))
;; => 6335

(let [s sample-data
      num-in-group (fn [g] (count (str/split-lines g)))]
  (->> (str/split s #"\n\n")
       (map #(assoc (frequencies %) :num-in-group (num-in-group %)))
       (map #(dissoc % \newline))
       (map #(dissoc (into {} (filter (fn [[k v]]
                                        (= v (:num-in-group %)))
                                      %))
                     :num-in-group))
       (map #(count %))
       (reduce +)
       ))
;; => 6

(let [s data
      num-in-group (fn [g] (count (str/split-lines g)))]
  (->> (str/split s #"\n\n")
       (map #(assoc (frequencies %) :num-in-group (num-in-group %)))
       (map #(dissoc % \newline))
       (map #(dissoc (into {} (filter (fn [[k v]]
                                        (= v (:num-in-group %)))
                                      %))
                     :num-in-group))
       (map #(count %))
       (reduce +)
       ))
;; => 3392

;; transducer version
(defn xf-with [num-in-group]
  (comp
   (map #(assoc (frequencies %) :num-in-group (num-in-group %)))
   (map #(dissoc % \newline))
   (map #(dissoc (into {} (filter (fn [[_ v]]
                                    (= v (:num-in-group %)))
                                  %))
                 :num-in-group))
   (map #(count %)))
  )

(let [s data
      num-in-group (fn [g] (count (str/split-lines g)))
      xf (xf-with num-in-group)]
  (transduce xf + (str/split s #"\n\n")))
;; => 3392
