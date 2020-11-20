(ns samhan.advent-of-code-2019.day-2)

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
