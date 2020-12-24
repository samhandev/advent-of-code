(ns aoc-2020.day24
  (:require [aoc-2020.utils :as utils]
            [clojure.string :as str]))

;; Details on hex grids
;; https://catlikecoding.com/unity/tutorials/hex-map/part-1/
;;

(def small-sample "esenee")

(def data (utils/load-data 2020 24))

(def sample-data "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew")

(def movements {"ne" [0  -1 1]
                "e" [1 -1 0]
                "se" [1 0 -1]
                "sw" [0 1 -1]
                "w" [-1 1 0]
                "nw" [-1 0 1]})

(defn parse-line [l]
  (re-seq #"nw|sw|se|ne|e|w" l))

(defn parse [s]
  (->> (str/split-lines s)
       (map parse-line)))

(defn move [pos direction]
  (mapv + pos (movements direction)))

(def flip-self "nwwswee")

(defn ending [directions]
  (reduce move [0 0 0] directions))

(defn part1 []
  (let [directions (parse data)]
    (->> directions
         (map ending)
         (group-by identity)
         (filter (fn [[k v]] (odd? (count v))))
         count)))

(part1)
;; => 269

(defn init-board [data]
  (let [directions (parse data)]
    (reduce (fn [board tile]
              (if (contains? board tile)
                (disj board tile)
                (conj board tile))) #{} (map ending directions))))

(count (init-board data))
;; => 269

(defn neighbours [pos]
  (map (partial move pos) (keys movements)))

(neighbours [0 0 0])

(let [sample-board (init-board sample-data)]
  (count sample-board))

;; reuse day17 game of life
(defn step [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (or (and (= n 2) (not (cells loc)))
                       (and (= n 1) (cells loc))
                       (and (= n 2) (cells loc)))]
         loc)))

(defn black-tiles-on-day [data day]
  (let [sample-board (init-board data)]
    (count (last (take (inc day) (iterate step sample-board))))))

(black-tiles-on-day sample-data 100)
;; => 2208

(black-tiles-on-day data 100)
;; => 3667
