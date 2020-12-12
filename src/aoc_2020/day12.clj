(ns aoc-2020.day12
  (:require [aoc-2020.utils :as utils]
            [clojure.string :as str]))

(def data (utils/load-data 2020 12))

(def sample-data "F10
N3
F7
R90
F11")

(defn parse-line [l]
  (let [[_ action value] (re-find #"([A-Z])(\d+)" l)]
    [(first action) (Integer/parseInt value)]))

(defn parse [s]
  (->> (str/split-lines s)
       (map parse-line)))

(def headings [\N \E \S \W])

(defn right-angle-turns [degrees]
  (/ degrees 90))

(defn new-heading [[action value] heading]
  (let [direction ({\R + \L -} action)]
    (get headings
         (mod (direction (.indexOf headings heading)
                         (right-angle-turns value)) 4))))

(defn update-pos [state [action value :as act-val]]
  (case action
    \N (update-in state [:pos 1] + value)
    \S (update-in state [:pos 1] - value)
    \E (update-in state [:pos 0] + value)
    \W (update-in state [:pos 0] - value)
    \F (update-pos state [(:heading state) value])
    \R (update state :heading (partial new-heading act-val))
    \L (update state :heading (partial new-heading act-val))))

(defn manhattan-dist [{:keys [pos]}]
  (->> pos
       (map (fn [x] (Math/abs x)))
       (apply +)))

(let [s data
      d (parse s)
      init {:pos [0 0]
            :heading \E}]
  (->> d
       (reduce update-pos init)
       manhattan-dist))
;; => 1601

;; Part 2

;; how does the rotating the waypoint around the ship work

;; east 10 north 4 = [10 4]
;; rotate 90 clockwise
;; east 4 10 south = [4 -10]

(defn rotate-waypoint [[action value] [x y :as waypoint]]
  (if (zero? value)
    waypoint
    (case action
      \R (rotate-waypoint [action (- value 90)] [y     (- x)])
      \L (rotate-waypoint [action (- value 90)] [(- y)    x]))))

(rotate-waypoint [\R 90] [10 4])

(defn move-to-way-point [[action value] {:keys [waypoint]} pos]
  (let [[x y] waypoint
        delta [(* x value) (* y value)]]
    (map +
         delta
         pos)))

(defn update-pos-p2 [state [action value :as act-val]]
  (case action
    \N (update-in state [:waypoint 1] + value)
    \S (update-in state [:waypoint 1] - value)
    \E (update-in state [:waypoint 0] + value)
    \W (update-in state [:waypoint 0] - value)
    \F (update state :pos (partial move-to-way-point act-val state))
    \R (update state :waypoint (partial rotate-waypoint act-val))
    \L (update state :waypoint (partial rotate-waypoint act-val))))

(let [s data
      d (parse s)
      init {:waypoint [10 1]
            :pos [0 0]}]
  (->> d
       (reduce update-pos-p2 init)
       manhattan-dist))
;; => 13340
