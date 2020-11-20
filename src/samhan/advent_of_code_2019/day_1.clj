(ns samhan.advent-of-code-2019.day-1)

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
