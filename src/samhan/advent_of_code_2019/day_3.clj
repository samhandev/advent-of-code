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
         positions []
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
               :positions (concat positions (:positions x))}))
          {:current {:x 0 :y 0}
           :positions [{:x 0 :y 0}]}
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

(defn xpaths [wire-positions]
  (disj (clojure.set/intersection
         (set (get-in wire-positions [:wire-1 :positions]))
         (set (get-in wire-positions [:wire-2 :positions])))
        {:x 0 :y 0}))

(def toy-data-2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

(def simple-data "R8,U5,L5,D2
U7,R6,D4,L3")

(defn part-1 []
  (-> (slurp "resources/3_input.txt")
      map-wire-positions-from-input
      closest-xpath))

(defn part-2 [input]
  (let [wire-pos (map-wire-positions-from-input input)
        xp (xpaths wire-pos)
        step-coords (concat (filter #(contains? xp (second %))
                                    (map-indexed vector (get-in wire-pos [:wire-1 :positions])))
                            (filter #(contains? xp (second %))
                                    (map-indexed vector (get-in wire-pos [:wire-2 :positions]))))]
    (second
     (apply min-key second
            (reduce (fn [acc [steps coord]]
                      (update acc coord (fnil + 0) steps))
                    {} step-coords)))))

(part-1)
;; => 806

(part-2 (slurp "resources/3_input.txt"))
;; => 66076

(comment
  (def example-2-1 "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83")

  (def day-3-input (slurp "resources/3_input.txt"))

  (def test-1
    (map-wire-positions-from-input example-2-1))

  (closest-xpath test-1)

  (positions-moved-to {:x 5 :y 5} {:direction \L :distance 2})
;; => {:current {:x 3, :y 5}, :positions [{:x 5, :y 5} {:x 4, :y 5} {:x 3, :y 5}]}

  )
