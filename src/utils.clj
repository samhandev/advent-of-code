(ns aoc-2020.utils
  (:require [clojure.java.io :as io]))


(defn load-data [year day]
  (slurp (io/resource (str "aoc_" year "/day_" day ".txt"))))

