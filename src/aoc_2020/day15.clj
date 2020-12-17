(ns aoc-2020.day15
  (:require [clojure.string :as str]))

(def sample-dataa "1,3,2")

(def sample-data "0,3,6")

(defn parse [s]
  (map #(Integer/parseInt %) (str/split s #",")))

(defn init-state [data]
  (let [starters (parse data)]
    {:spoken (into {} (map-indexed (fn [^long i v] [v (inc i)]) starters))
     :turn (inc (count starters))
     :last-spoken 0}))

(init-state sample-data)
;; => {:spoken {0 1, 3 2, 6 3}, :turn 4, :last-spoken 0}

(defn next-game-state [{:keys [spoken ^long turn last-spoken] :as state}]
  (if-let [^long previously (get spoken last-spoken)]
    (-> state
        (assoc :last-spoken (- turn previously))
        (update :spoken assoc last-spoken turn)
        (update :turn inc))
    (-> state
        (assoc :last-spoken 0)
        (update :spoken assoc last-spoken turn)
        (update :turn inc)
        )
    )
  )

(comment
  (take 10 (iterate next-game-state (init-state sample-data)))


  (map #(select-keys % [:turn :last-spoken])
       (take 5 (drop 2016 (iterate next-game-state (init-state sample-data)))))

  (def data "9,6,0,10,18,2,1")

  (map #(select-keys % [:turn :last-spoken])
       (take 5 (drop 2010 (iterate next-game-state (init-state data))))))
;; => ({:turn 2018, :last-spoken 24}
;;     {:turn 2019, :last-spoken 157}
;;     {:turn 2020, :last-spoken 1238}
;;     {:turn 2021, :last-spoken 0}
;;     {:turn 2022, :last-spoken 6})


(comment
  (time
   (mapv #(select-keys % [:turn :last-spoken])
         (take 5 (drop (- 30000000 10) (iterate next-game-state (init-state "9,6,0,10,18,2,1"))))))
;; => [{:turn 29999998, :last-spoken 6466}
;;     {:turn 29999999, :last-spoken 44015}
;;     {:turn 30000000, :last-spoken 3745954}
;;     {:turn 30000001, :last-spoken 0}
;;     {:turn 30000002, :last-spoken 10}]

 ;; takes ~ 22 seconds
  )
