(ns samhan.advent-of-code-2019.day-3)

(defn parse-wire-input [input]
  {:direction (first input)
   :distance (Integer/parseInt (apply str (rest input)))})

(parse-wire-input "R78")

(comment
  (def central-port {:x 0 :y 0})

  (def example-2-1 "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83")

  (def day-3-input (slurp "resources/3_input.txt"))

  (defn input-3 [input] (clojure.string/split input #"\n"))

  (let [[wire-1 wire-2] (input-3 example-2-1)
        parse-wire-inputs (fn [x] (->> (clojure.string/split x #",")
                                       (mapv parse-wire-input)))]
    {:wire-1 (parse-wire-inputs wire-1)
     :wire-2 (parse-wire-inputs wire-2)})

  (defn update-positions [positions curr-position movement]
    (let [new-positions ()]))

  ;; if right we increase the x axis

  {:x 5 :y 5}

  (defn positions-moved-to [direction start distance]
    (let [dir-op ({\R +
                   \L -
                   \U +
                   \D -} direction)]
      (range (dir-op start 1) (dir-op start distance 1))))

  (positions-moved-to \R 5 10)
;; => (6 7 8 9 10 11 12 13 14 15)
  (positions-moved-to \L 5 10)
;; => ()

  )
