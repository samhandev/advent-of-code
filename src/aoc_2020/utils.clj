(ns aoc-2020.utils
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn load-data [year day]
  (slurp (io/resource (str "aoc_" year "/day_" day ".txt"))))

(defn parse [s]
  (map edn/read-string (str/split-lines s)))

(defn load-data-nums [year day]
  (parse (load-data year day)))
