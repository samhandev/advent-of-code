(ns aoc-2020.day3
  (:require [clojure.string :as str]
            [aoc-2020.utils :as utils]))

(def toy-data "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(defn parse [s]
  (let [d (str/split-lines s)
        depth (count d)
        width (count (first d))]
    {:v d
     :depth depth
     :width width}))

(defn tree? [{:keys [v width] :as terrain} [x y]]
  (= (-> v
         (get y)
         (get (rem x width)))
     \#))

(defn count-hit-trees [terrain traversal]
  (loop [trees 0
         [x y] traversal]
    (if (> y (:depth terrain))
      trees
      (recur (if (tree? terrain [x y])
               (inc trees)
               trees)
             (mapv + [x y] traversal)))))


(count-hit-trees (parse toy-data) [3 1])
;; => 7

(def data (utils/load-data 2020 3))
(count-hit-trees (parse data) [3 1])

(def traversals [[1 1]
                 [3 1]
                 [5 1]
                 [7 1]
                 [1 2]])

(apply * (map (partial count-hit-trees (parse data)) traversals))
;; => 2122848000
