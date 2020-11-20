(ns samhan.advent-of-code-2019.day-3)

(defn parse-wire-input [input]
  {:direction (first input)
   :distance (Integer/parseInt (apply str (rest input)))})

(parse-wire-input "R78")

(defn parse-input [input] (clojure.string/split input #"\n"))

(defn mov [pos move]
  (condp = (:direction move)
    \R (update pos :x inc)
    \L (update pos :x dec)
    \U (update pos :y inc)
    \D (update pos :y dec)))

(defn positions-moved-to [curr movement]
  (loop [curr curr
         positions [curr]
         movement movement]
    (if (= 0 (:distance movement))
      {:current (last positions)
       :positions positions}
      (let [new-curr (mov curr movement)]
        (recur new-curr
               (conj positions new-curr)
               (update movement :distance dec))))))

(defn map-positions [movements]
  (reduce (fn [{:keys [current positions]} movement]
            (let [x (positions-moved-to current movement)]
              {:current (:current x)
               :positions (concat (:positions x) positions)}))
          {:current {:x 0 :y 0}
           :positions []}
          movements))

(defn map-wire-positions-from-input [input]
  (let [[wire-1 wire-2] (parse-input input)
        parse-wire-inputs (fn [x] (->> (clojure.string/split x #",")
                                       (mapv parse-wire-input)))
        wire-1-movements (parse-wire-inputs wire-1)
        wire-2-movements (parse-wire-inputs wire-2)]
    {:wire-1 (map-positions wire-2-movements)
     :wire-2 (map-positions wire-1-movements)}))

(defn closest-xpath [wire-positions]
  (apply min
         (map (fn [{:keys [x y]}] (+ (Math/abs x)
                                     (Math/abs y)))
              (disj (clojure.set/intersection
                     (set (get-in wire-positions [:wire-1 :positions]))
                     (set (get-in wire-positions [:wire-2 :positions])))
                    {:x 0 :y 0}))))

(comment
  (def example-2-1 "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83")

  (def day-3-input (slurp "resources/3_input.txt"))

  (def test-1
    (map-wire-positions-from-input example-2-1))

  (closest-xpath test-1)

  (positions-moved-to {:x 5 :y 5} {:direction \L :distance 2})
;; => {:current {:x 3, :y 5}, :positions [{:x 5, :y 5} {:x 4, :y 5} {:x 3, :y 5}]}

  (-> day-3-input
      map-wire-positions-from-input
      closest-xpath)

  )
