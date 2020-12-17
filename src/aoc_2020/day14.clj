(ns aoc-2020.day14
  (:require [aoc-2020.utils :as utils]
            [clojure.pprint :refer [cl-format]]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def sample-data "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(def data (utils/load-data 2020 14))

(defn parse-line [l]
  (let [[action value] (str/split l #" = ")]
    (cond
      (str/starts-with? action "mask")
      {:action action
       :init-mask value
       :value (keep-indexed (fn [i v] (when (not= \X v)
                                        [(Integer/parseInt (str v)) i])) (reverse value))
       :floating (keep-indexed (fn [i v] (when (= \X v)
                                           i)) (reverse value))}

      (str/starts-with? action "mem")
      {:action "mem"
       :location (Long/parseLong (re-find #"\d+" action))
       :value (Long/parseLong value)}
      )))

;; => "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
(defn parse-data [data]
  (let [lines (str/split-lines data)]
    (mapv parse-line lines)))

(def parts-sample (parse-data sample-data))
parts-sample

;; => ({:action "mask", :value "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"}
;;     {:action "mem", :location 8, :value 11}
;;     {:action "mem", :location 7, :value 101}
;;     {:action "mem", :location 8, :value 0})

;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
(defn parse-mask-value [value]
  (keep-indexed (fn [i v] (when (not= \X v)
                            [(Integer/parseInt (str v)) i])) (reverse value)))

(parse-mask-value "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")

(defn apply-mask [mask value]
  (reduce (fn [v [bit idx]]
            (case (long bit)
              0 (bit-clear v idx)
              1 (bit-set v idx)))
          value mask))

(parse-data sample-data)
;; => [{:action "mask", :value ([0 1] [1 6])}
;;     {:action "mem", :location 8, :value 11}
;;     {:action "mem", :location 7, :value 101}
;;     {:action "mem", :location 8, :value 0}]

(apply-mask '([0 1] [1 6]) 0)

(let [d (parse-data data)]
  (loop [state {:pc 0
                :memory {}
                :program d
                :mask []}]
    (if (>= ^int (:pc state) ^int (count (:program state)))
      (apply + (vals (:memory state)))
      (let [{:keys [action value location]} (get (:program state) (:pc state))]
        (case action
          "mask" (recur (-> state
                            (assoc :mask value)
                            (update :pc inc)))
          "mem" (recur (-> state
                           (assoc-in [:memory location] (apply-mask (:mask state) value))
                           (update :pc inc))))))))
;; => 4886706177792


;; given a value and a mask, return an array with the other possible values
;; truth table bit-and
;; for each

(defn set-mask [mask s]
  (reduce (fn [agg x]
            (str/replace-first agg #"X" (str x)))
          mask s))

(defn binary-permutation
  "http://www.gigamonkeys.com/book/a-few-format-recipes.html
  ~v indicates it takes an argument,
  `0b` indicates leading `0` binary number formating
  "
  [n]
  (map (partial cl-format nil "~v,'0b" n) (range 0 (Math/pow 2 n))))

(def sample-p2-data "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")

(defn gen-addresses [mask]
  (let [x-count (count (filter #(= \X %) mask))]
    (map #(set-mask mask %) (binary-permutation x-count))))

(defn apply-mask-with-floating [mask value]
  (let [padded-binary (cl-format nil "~36,'0b" value)]
    (apply str (map (fn [b m] [b m]
                      (if (#{\1 \X} m)
                        m
                        b)
                      ) padded-binary mask))))

(gen-addresses (apply-mask-with-floating "000000000000000000000000000000X1001X" 42))
;; => ("000000000000000000000000000000011010"
;;     "000000000000000000000000000000011011"
;;     "000000000000000000000000000000111010"
;;     "000000000000000000000000000000111011")

(defn update-state-mem [state locs value]
  (reduce (fn [s loc] (assoc-in s [:memory loc] value)) state locs))

(time
 (let [d (parse-data data)]
   (loop [state {:pc 0
                 :memory {}
                 :program d
                 :mask nil}]
     (if (>= ^int (:pc state) ^int (count (:program state)))
       (apply + (vals (:memory state)))
       (let [{:keys [action value location init-mask]} (get (:program state) (:pc state))]
         (case action
           "mask" (recur (-> state
                             (assoc :mask init-mask)
                             (update :pc inc)))
           "mem" (let [mem-locs (gen-addresses (apply-mask-with-floating (:mask state) location))]
                   (recur (-> state
                              (update-state-mem mem-locs value)
                              (update :pc inc))))))))))
