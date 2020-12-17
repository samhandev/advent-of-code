(ns aoc-2020.day16
  (:require [aoc-2020.utils :as utils]
            clojure.pprint
            [clojure.string :as str]))

(def sample-data "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")

:input "class: 1-3 or 5-7"
:output {:name "class", :s1 "1", :s2 "5", :e1 "3", :e2 "7"}
(defn parse-rule [l]
  (let [[_ rule s1 e1 s2 e2] (re-find #"([a-z|\s]+): (\d+)-(\d+) or (\d+)-(\d+)" l)
        [s1 e1 s2 e2] (map #(Integer/parseInt %) [s1 e1 s2 e2])]
    {:name rule
     :s1 s1
     :s2 s2
     :e1 e1
     :e2 e2}))

(parse-rule "class test: 1-3 or 5-7")


(defn parse-ticket [l]
  (map #(Integer/parseInt %) (str/split l #",")))

(defn parse [s]
  (let [[rules your-ticket nearby-ticket] (str/split s #"\n\n")
        rules' (->> rules
                    (str/split-lines)
                    (map parse-rule))
        your-ticket' (-> your-ticket
                         (str/split-lines)
                         second
                         parse-ticket)
        nearby-ticket' (->> nearby-ticket
                            (str/split-lines)
                            rest
                            (map parse-ticket))]
    {:rules rules'
     :your-ticket your-ticket'
     :nearby-ticket nearby-ticket'}))

:input {:name "class", :s1 1, :s2 5, :e1 3, :e2 7}
:input 7
(defn valid-field? [{:keys [s1 e1 s2 e2]} field]
  (let [rule-fn (fn [x] (or (<= s1 x e1)
                            (<= s2 x e2)))]
    (rule-fn field)))

(valid-field? {:name "class", :s1 1, :s2 5, :e1 3, :e2 7} 4)

(parse sample-data)
;; => {:rules
;;     ({:name "class", :s1 1, :s2 5, :e1 3, :e2 7}
;;      {:name "row", :s1 6, :s2 33, :e1 11, :e2 44}
;;      {:name "seat", :s1 13, :s2 45, :e1 40, :e2 50}),
;;     :your-ticket (7 1 14),
;;     :nearby-ticket ((7 3 47) (40 4 50) (55 2 20) (38 6 12))}

(def data (utils/load-data 2020 16))

(defn part1 [data]
  (let [{:keys [rules nearby-ticket]} (parse data)
        fields (flatten nearby-ticket)
        rule-fns (map #(partial valid-field? %) rules)
        pass-any-rule-fn (apply some-fn rule-fns)
        fail-any-rule-fn (complement pass-any-rule-fn)]
    (reduce + (filter fail-any-rule-fn fields))))

(part1 data)
;; => 30869

(defn every-field-pass-a-rule [ticket]
  (every? (fn [[_ _ rules-passed]]
            (not-empty rules-passed))
          ticket))

(defn index-valid-rules [data]
  (let [{:keys [rules nearby-ticket]} (parse data)
        rule-fns (map (fn [rule]{:name (:name rule)
                                 :rule-fn (partial valid-field? rule)})
                      rules)
        passing-rules (fn [ticket] (map-indexed (fn [i v]
                                                  [i v (reduce (fn [agg {:keys [name rule-fn]}]
                                                                 (if (rule-fn v)
                                                                   (conj agg name)
                                                                   agg)) [] rule-fns)]) ticket))
        parsed-tickets (->> (map passing-rules nearby-ticket)
                            (filter every-field-pass-a-rule))
        index-valid-rules (for [i (range (count (first nearby-ticket)))]
                            {i (->> parsed-tickets
                                    (map (comp set last #(nth % i)))
                                    (apply clojure.set/intersection))})]
    (apply merge index-valid-rules)))

(frequencies (mapcat (fn [[k v]] (vec v)) (index-valid-rules data)))

(defn field-freqs [ivr]
  (frequencies (mapcat (fn [[k v]]
                         (vec v))
                       ivr)))

(defn idx-of-field [ivr rule]
  (ffirst (filter (fn [[k v]] (contains? v rule)) ivr)))

(defn only-rule [field-freqs]
  (ffirst (filter (fn [[k v]] (= 1 v)) field-freqs)) )

(defn idx-to-rule [data]
  (loop [ivr (index-valid-rules data)
         found-rule-idx {}]
    (if (empty? ivr)
      found-rule-idx
      (let [field-freqs (field-freqs ivr)
            only-rule (only-rule field-freqs)
            idx (idx-of-field ivr only-rule)]
        ;;find the index with the only-rule and set it
        ;; recur
        (recur (dissoc ivr idx) (assoc found-rule-idx idx only-rule))))))

(defn part2 []
  (let [parsed-data (parse data)
        departure-fields (filter (fn [[k v]] (str/starts-with? v "departure")) (idx-to-rule data))
        departure-idx (map first departure-fields)]
    (reduce * (map (fn [n] (nth (:your-ticket parsed-data) n)) departure-idx))))

(part2)
;; => 4381476149273
