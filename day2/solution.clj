(require '[clojure.string :as str])

;; Part one (find safe levels)
;; 1. Split each line into a "levels" string
;; 2. For each "levels" string
;; 2.1 parse the contents into integers
;; 2.2 loop contents in pairs
;; 2.2.1 get the abs difference between the pair
;; 2.2.2 get the direction by comparing the pair
;; 2.2.3 check that the difference is within "safe" limits
;; 2.2.4 check that the direction has not changed
;; 3. Reduce the input lines into a total of safe levels

(defn direction-changed? [prev curr]
  (cond
    (nil? prev) false
    (not= curr prev) true
    :else false))

(defn check-level [level]
  (loop [prev-direction nil
         level-values (map Integer/parseInt (str/split level #" "))]
    (let [a (first level-values)
          b (second level-values)
          difference (if (nil? b) 1 (abs (- a b)))
          direction (if (nil? b) prev-direction (compare a b))]
      (cond
        (< (count level-values) 2) 1
        (< difference 1) 0
        (> difference 3) 0
        (direction-changed? prev-direction direction) 0
        :else (recur direction (rest level-values))))))

(defn safe-reports-amount [input]
  (reduce (fn [safe-count level] (+ safe-count (check-level level))) 0 (str/split-lines input)))

(println (safe-reports-amount (slurp "input.txt")))
