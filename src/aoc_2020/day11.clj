(ns aoc-2020.day11
  (:require [aoc-2020.utils :as utils]
            [clojure.string :as str]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

(def sample-data "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def data (utils/load-data 2020 11))

(defn parse [s]
  (let [d (mapv vec (str/split-lines s))]
    {:width (count (first d))
     :height (count d)
     :data d}))

(defn occupied-seat? [seat]
  (= \# seat))

(defn neighbours [^long width ^long height [^long x ^long y]]
  (for [^long x' [-1 0 1]
        ^long y'[-1 0 1]
        :when (not= 0 x' y')
        :let [x'' (+ x x')
              y'' (+ y y')]
        :when (and (>= x'' 0)
                   (>= y'' 0)
                   (< x'' height)
                   (< y'' width))]
    [x'' y'']))

(def m-neighbours (memoize neighbours))

(defn get-seat [data [x y]]
  (get (get data x) y))

(defn adjacent-seats [{:keys [width height data ^long seat-limit]} pos]
  (let [adj (m-neighbours width height pos)]
    (reduce (fn [^long c pos]
              (if (>= c seat-limit)
                (reduced c)
                (let [seat (get-seat data pos)]
                  (if (= \# seat)
                    (inc c)
                    c))))
            0
            adj)))

(defn next-round-mapping [{:keys [data seat-fn seat-limit] :as d}]
  (assoc d :data
         (mapv (fn [row x]
                 (mapv (fn [seat y]
                         (let [adj-occupied (seat-fn d [x y])]
                           (case seat
                             \L (if (= 0 adj-occupied)
                                  \#
                                  \L)
                             \# (if (>= ^long adj-occupied ^long seat-limit)
                                  \L
                                  \#)
                             seat)))
                       row
                       (range 0 (count row))))
               data
               (range 0 (count data)))))

(defn count-seats [{:keys [data]}]
  (count (filter occupied-seat? (flatten data))))


(defn runner-loop [d]
  (loop [n 0
         prev-state d
         next-state (next-round-mapping d)]
    (if (= (:data prev-state)
           (:data next-state))
      {:seat-count (count-seats next-state)
       :iterations n}
      (recur (inc n) next-state (next-round-mapping next-state)))))

(comment
  "part 1"
  (time
   (runner-loop (assoc (parse data)
                  :seat-fn adjacent-seats
                  :seat-limit 4)))
  ;; => 2273
)

(def deltas
  (for [^long x [-1 0 1]
        y (if (zero? x) [-1 1] [-1 0 1])]
    [x y]))

(defn out-of-bounds? [^long width ^long height [^long x ^long y]]
  (if (or (< x 0 ) (< y 0) (>= x height) (>= y width))
    true
    false))

(defn line-of-sight [{:keys [width height data]} pos delta]
  (loop [curr (map + pos delta)]
    (if (out-of-bounds? width height curr)
      nil
      (let [seat (get-seat data curr)]
        (if (#{\L \#} seat)
          seat
          (recur (map + curr delta)))))))

(defn line-of-sight-seats [d pos]
  (reduce (fn [^long cnt delta]
            (if (>= cnt ^long (:seat-limit d))
              (reduced cnt)
              (if (#{\#} (line-of-sight d pos delta))
                (inc cnt)
                cnt)))
          0
          deltas))

(def p2-sample (assoc (parse sample-data)
                      :seat-fn line-of-sight-seats
                      :seat-limit 5))

(def p2-data (assoc (parse data)
                    :seat-fn line-of-sight-seats
                    :seat-limit 5))

(defn run-part-2-sample [opt]
  (runner-loop p2-sample))

(defn run-part-2 [opt]
  (runner-loop p2-data))

(comment
  (profile
   {}
   (runner-loop p2-data))

  (time
   (runner-loop p2-data))
;; => 2064
  )

(defn view [data]
  (print "\033[2J")
  (println
   (->> data (str/join "\n")))
  (Thread/sleep 250))
