(ns aoc-2020.day4
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]
            [clojure.walk :as cw]))

(def sample-data "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def sample-1 (let [s sample-data]
                (str/split s #"\n\n")))

(defn parse-record [l]
  (->> (str/split l #"[ |\n]")
       (map #(str/split % #"\:"))
       (into {})
       (cw/keywordize-keys)))

(defn parse [d]
  (let [d' (str/split d #"\n\n")]
    (map parse-record d')))

(let [l (first sample-1)]
  (->> (str/split l #"[ |\n]")
       (map #(str/split % #"\:"))
       (into {})))

(let [l (nth sample-1 2)]
(str/split l #"[ |\n]"))

(parse sample-data)

(defn required-keys? [p]
  (every? #(contains? p %) [:ecl :pid :eyr :hcl :byr :iyr :hgt]))

(defn passport-valid? [p]
  (let [valid? (every-pred required-keys?)]
    (valid? p)))

(let [passports (parse sample-data)]
  (map required-keys? passports))

(def data (slurp "resources/aoc_2020/day_4.txt"))

(let [passports (parse data)]
  (count (filter passport-valid? passports)))
;; => 242

(deftest part-1
  (is (= 242 (->> data
                  parse
                  (filter passport-valid?)
                  count))))

(defn valid-num? [s minimum maximum]
  (let [year (Integer/parseInt s)]
    (<= minimum year maximum)))

(defn byr? [byr]
  (valid-num? byr 1920 2002))

(byr? "2002")
;; => true
(byr? "2003")
;; => false

(defn iyr? [iyr]
  (valid-num? iyr 2010 2020))

(defn eyr? [eyr]
  (valid-num? eyr 2020 2030))

(defn hgt? [hgt]
  (let [num-string (second (re-find #"^(\d+)" hgt))]
    (cond (str/ends-with? hgt "cm") (valid-num? num-string 150 193)
          (str/ends-with? hgt "in") (valid-num? num-string 59 76)
          :else false)))
(hgt? "60in")
;; => true
(hgt? "190cm")
;; => true
(hgt? "190in")
;; => false
(hgt? "190")
;; => false

(defn hcl? [hcl]
  (re-matches #"\#[0-9a-f+]{6}" hcl))
(hcl? "#123abc")
;; => "#123abc"
(hcl? "#123abz")
;; => nil
(hcl? "123abc")
;; => nil

(defn ecl? [ecl]
  (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl))
(ecl? "brn")
;; => true
(ecl? "wat")
;; => false

(defn pid? [pid]
  (re-matches #"[0-9+]{9}" pid))
(pid? "000000001")
;; => "000000001"

(pid? "0123456789")
;; => nil


(defn valid-passport? [p]
  (let [validations {
                     :byr byr?
                     :iyr iyr?
                     :eyr eyr?
                     :hgt hgt?
                     :hcl hcl?
                     :ecl ecl?
                     :pid pid?
                     }]

    (and (required-keys? p)
         (every? (fn [[k v]]
                   (let [v-fn (get validations k (constantly false))]
                     (v-fn v)))
                 (dissoc p :cid)))))

(def sample-invalid "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")


(deftest test-sample-invalid
  (is (= [false false false false]
         (let [d (parse sample-invalid)]
               (map valid-passport? d)))))



(let [d (parse sample-invalid)]
  (map valid-passport? d))

(def sample-valid "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022")

(deftest test-sample-valid
  (is (= [true true true]
         (let [d (parse sample-valid)]
           (map valid-passport? d)))))


(deftest part-2
  (is (= 186 (->> data
                  parse
                  (filter valid-passport?)
                  count))))

(run-tests)

(s/def ::byr byr?)
(s/def ::iyr iyr?)
(s/def ::eyr eyr?)
(s/def ::hgt hgt?)
(s/def ::hcl hcl?)
(s/def ::ecl ecl?)
(s/def ::pid pid?)
(s/def ::cid string?)

(s/def ::passport (s/keys :req-un [::byr
                                   ::iyr
                                   ::eyr
                                   ::hgt
                                   ::hcl
                                   ::ecl
                                   ::pid]
                          :opt-un [::cid]))

(s/valid? ::passport (first (parse sample-data)))

(deftest part-2-spec
  (is (= 186 (->> data
                  parse
                  (filter (partial s/valid? ::passport))
                  count))))
