(ns aoc-2020.day8
  (:require [aoc-2020.utils :as utils]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def sample-data "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn parse [s]
  (->> (str/split-lines s)
       (map (fn [l]
              (let [[op v](str/split l #" ")]
                [op (edn/read-string v)])))))


(defn apply-operation [{:keys [data counter] :as mem}]
  (let [[op v] (nth data counter)
        mem' (update mem :seen conj counter)]
    (case op
      "acc" (-> mem'
                (update :acc + v)
                (update :counter inc))
      "nop" (-> mem'
                (update :counter inc))
      "jmp" (-> mem'
                (update :counter + v)))))


(defn iterate-till-loop [d]
  (let [mem {:seen #{}
             :counter 0
             :acc 0
             :data d}]
    (loop [mem mem]
      (cond (contains? (:seen mem) (:counter mem)) {:status :looped
                                                    :value (:acc mem)}
            (>= (:counter mem) (count (:data mem))) {:status :finished
                                                     :value (:acc mem)}
            :else (recur (apply-operation mem))))))


(def data (utils/load-data 2020 8))

(iterate-till-loop (parse sample-data))
;; => 5

(iterate-till-loop (parse data))
;; => 1654

(def sample-data-fixed "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
nop -4
acc +6")

(defn get-index-ops [d]
  (filter (fn [[_ op]] (#{"jmp" "nop"} op))
          (map-indexed (fn [idx [op _]] [idx op])
                       d)))

(iterate-till-loop (parse sample-data))

(def memorized-fn iterate-till-loop)

(defn finished? [result]
  (= :finished (:status result)))

(defn switch-op [[op value]]
  (if (= "nop" op)
    ["jmp" value]
    ["nop" value]))

(defn try-other [d [index _]]
  (update-in (vec d)
             [index]
             (fn [oper] (switch-op oper))))


(let [d (parse data)
      indexed-ops (get-index-ops d)]
  (loop [data d
         ops indexed-ops]
    (cond (finished? (memorized-fn (try-other data (first ops)))) (:value (memorized-fn (try-other data (first ops))))
          :else (recur d (rest ops)))))

