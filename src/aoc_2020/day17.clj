(ns aoc-2020.day17
  (:require [clojure.string :as str]))

(def data ".###..#.
##.##...
....#.#.
#..#.###
...#...#
##.#...#
#..##.##
#.......")

;; based on http://clj-me.cgrand.net/2011/08/19/conways-game-of-life/

(defn parse [data]
  (let [lines (str/split-lines data)]
    (set (keep identity (apply concat (map-indexed (fn [y line]
                                                     (map-indexed (fn [x v]
                                                                    (when (= \# v)
                                                                      [x y 0])) line)) lines))))))

(parse data)

(defn neighbours-3d [[x y z]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        :when (not= 0 dx dy dz)]
    [(+ dx x) (+ dy y) (+ dz z)]))

(defn step [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours-3d cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(def board #{[1 0 0] [2 1 0] [0 2 0] [1 2 0] [2 2 0]})

(count (first (drop 6 (iterate step board))))
;; => 112

(count (first (drop 6 (iterate step (parse data)))))
;; => 304


;; part 2
(defn parse-p2 [data]
  (let [lines (str/split-lines data)]
    (set (keep identity (apply concat (map-indexed (fn [y line]
                                                     (map-indexed (fn [x v]
                                                                    (when (= \# v)
                                                                      [x y 0 0])) line)) lines))))))


(defn neighbours-4d [[x y z w]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        dw [-1 0 1]
        :when (not= 0 dx dy dz dw)]
    [(+ dx x) (+ dy y) (+ dz z) (+ dw w)]))

(defn step-p2 [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours-4d cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(count (first (drop 6 (iterate step-p2 (parse-p2 data)))))
