(ns aoc-2020.day7
  (:require [clojure.string :as str]
            [aoc-2020.utils :as utils]))

(def sample-data "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defn parse-line
  "Inverts inner colour as the key"
  [agg l]
  (let [[_ colour contains-bags] (re-find #"([a-z ]+) bags contain (.+)" l)
        bags (re-seq #"(\d+) (.+?) bags?[,.]" contains-bags)]
    (reduce (fn [agg [_ num inner-colour]]
              (update agg inner-colour conj colour))
            agg
            bags)))

(defn bag-graph [data]
  (->> data
       (str/split-lines)
       (reduce parse-line {})))

(bag-graph sample-data)

(defn count-bags
  "Creates a set of bags contain `bag` recursivly"
  [graph bag]
  (let [inner-bags (get graph bag)]
    (if (nil? inner-bags)
      nil
      (reduce conj
              (set inner-bags)
              (mapcat #(count-bags graph %)
                      inner-bags)))))

(count-bags (bag-graph sample-data) "shiny gold")
;; => #{"muted yellow" "light red" "dark orange" "bright white"}

(def data (utils/load-data 2020 7))

(count (count-bags (bag-graph data) "shiny gold"))
;; => 179

;; part 2
(bag-graph sample-data)

(defn parse-with-num
  [agg l]
  (let [[_ colour contains-bags] (re-find #"([a-z ]+) bags contain (.+)" l)
        bags (re-seq #"(\d+) (.+?) bags?[,.]" contains-bags)]
    (assoc agg colour (reduce (fn [init [_ num bag-colour]]
                                (assoc init bag-colour (Integer/parseInt num))) {} bags))))

(defn bag-num-graph [data]
  (->> data
       (str/split-lines)
       (reduce parse-with-num {})))

(bag-num-graph sample-data)

(defn count-num-bags [graph bag]
  (let [inner-bags (get graph bag)]
    (if (empty? inner-bags)
      1
      (reduce + 1 (map (fn [[inner-colour num]]
                         (* num (count-num-bags graph inner-colour)))
                       inner-bags)))))

(->> "shiny gold"
     (count-num-bags (bag-num-graph sample-data))
     dec)
;; => 32

(->> "shiny gold"
     (count-num-bags (bag-num-graph data))
     dec)
;; => 18925
