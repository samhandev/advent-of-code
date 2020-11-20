(ns samhan.advent-of-code-2019)

;; day 1
(defn required-fuel [mass]
  (max (- (int (Math/floor (/ mass 3)))
          2)
       0))

(defn recur-required-fuel [mass]
  (loop [total-mass 0
         m (required-fuel mass)]
    (if (<= m 0)
      total-mass
      (recur (+ total-mass m)
             (required-fuel m)))))

(def inputs
  (->
   (slurp "resources/1_input.txt")
   (clojure.string/split #"\n")))

(def int-inputs
  (map (fn [x] (Integer/parseInt x))
       inputs))

(comment

  ;; day 1.1
  (reduce + (map required-fuel int-inputs))
  ;; => 3363760


  ;; day 1.2
  (reduce + (map recur-required-fuel int-inputs))
  ;; => 5042767

  )

;; day 2

(def instructions {1 +
                   2 *
                   99 :exit})

(defn parse-input [inputs noun verb]
  (let [prog-inputs (mapv #(Integer/parseInt %) (clojure.string/split inputs #","))]
    {:memory (assoc prog-inputs 1 noun 2 verb)}))

(defn process [base input-position]
  (let [prog-inputs (:memory base)
        [op address-1 address-2 address-result] (take 4 (drop input-position prog-inputs))
        op-fn (get instructions op)
        at-fn #(nth prog-inputs %)
        result (op-fn (at-fn address-1)
                      (at-fn address-2))]
    (assoc base :memory (assoc prog-inputs address-result result))))

(defn process-program [program]
  (loop [position 0
         prog program]
    (if (= 99 (nth (:memory prog) position))
      (first (:memory prog))
      (recur (+ position 4) (process prog position)))))

(defn initial-inputs [noun verb]
  (parse-input (clojure.string/trim-newline (slurp "resources/2_input.txt"))
               noun verb))

(def answer-2-2
  (first
   (filter
    #(= 19690720 (:result %))
    (for [noun (range 99)
          verb (range 99)]
      {:noun noun
       :verb verb
       :result (process-program (initial-inputs noun verb))}))))


(comment

  (let [{:keys [noun verb]} answer-2-2]
    (+ (* 100 noun)
       verb))
;; => 8298
  )


;; day 3

;; get all the points that the wire covers

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
