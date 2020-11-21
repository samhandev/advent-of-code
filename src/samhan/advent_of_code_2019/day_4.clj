(ns samhan.advent-of-code-2019.day-4)

(def input "264793-803935")

(defn parse-input [input]
  (let [[_ start end] (re-find #"(\d+)-(\d+)" input)]
    (range (Integer/parseInt start) (inc (Integer/parseInt end)))))

(defn increasing? [s]
  (let [parse-char #(Character/digit % 10)
        pairs (map (fn [x y] [x y]) s (rest s))]
    (->> pairs
         (every? (fn [[x y]]
                   (>= (parse-char y) (parse-char x)))))))

(defn at-least-one-double? [s]
  (let [pairs (map (fn [x y] [x y]) s (rest s))]
    (->> pairs
         (some (fn [[x y]] (= x y))))))

(defn includes-strict-pair? [s]
  (->> (partition-by identity s)
       (some #(= (count %) 2))))

(defn part-1 []
  (let [possible-inputs (parse-input input)]
    (->> (filter (fn [pi]
                   ((every-pred increasing?
                                at-least-one-double?)
                    (str pi)))
                 possible-inputs)
         count)))

(comment
  (part-1)
  ;; => 966
)

(defn part-2 []
  (let [possible-inputs (parse-input input)]
    (->> (filter (fn [pi]
                   ((every-pred increasing?
                                at-least-one-double?
                                includes-strict-pair?)
                    (str pi)))
                 possible-inputs)
         count)))

(comment
  (part-2)
  ;; => 628
)
