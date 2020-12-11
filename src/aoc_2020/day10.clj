(ns aoc-2020.day10
  (:require [aoc-2020.utils :as utils]))

(def sample-data "16
10
15
5
1
11
7
19
6
12
4")

(def sample-d (utils/parse sample-data))

(def data (utils/load-data-nums 2020 10))

(defn sorted-with-plug-n-device [d]
  (sort (conj d 0
              (+ 3 (apply max d)))))

(defn part-1 [d]
  (let [adapaters (sorted-with-plug-n-device d)
        {ones 1
         threes 3} (frequencies (map (fn [x y] (Math/abs (- x y)))
                                     (butlast adapaters)
                                     (rest adapaters)))]
    (* ones threes)))

(part-1 data)

(defn options [d]
  (let [[head & tail] (take 4 d)]
    (filter (fn [x] (< 3 (- head x)) tail))))

(defn within-3 [d target index]
  (<= (Math/abs (- target (nth d index -1))) 3))

(def calls (atom 0))

(def path-count
  (memoize (fn [d val index]
             (swap! calls inc)
             (if (= (dec (count d)) index)
               1
               (let [next-indicies (range (inc index) (+ (inc index) 3))
                     valid-indicies (keep (fn [i] (when (within-3 d val i)
                                                    i))
                                          next-indicies)]
                 (apply + (map #(path-count d (nth d %) %) valid-indicies)))
               ))))

(comment
  (time
   (path-count (vec (sorted-with-plug-n-device data))
               0
               0)
   )
  )
