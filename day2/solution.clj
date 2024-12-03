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

(defn parse-level [level]
  ;; use vec call here to un-lazy the sequence
  (vec
   (map Integer/parseInt (str/split level #" "))))

(defn direction-changed? [prev curr]
  (cond
    (nil? prev) false
    (not= curr prev) true
    :else false))

(defn check-level [level]
  (loop [prev-direction nil
         index 0
         remaining level]
    (let [a (first remaining)
          b (second remaining)
          difference (if (nil? b) 1 (abs (- a b)))
          direction (if (nil? b) prev-direction (compare a b))]
      (cond
        (< (count remaining) 2) [true, nil]
        (< difference 1) [false, index]
        (> difference 3) [false, index]
        (direction-changed? prev-direction direction) [false, index]
        :else (recur direction (+ index 1) (rest remaining))))))

(defn safe-reports-amount [input]
  (reduce
   (fn [safe-count level] (+ safe-count (if (first (check-level (parse-level level))) 1 0)))
   0
   (str/split-lines input)))

(println (safe-reports-amount (slurp "input.txt")))

;; Part two (dampener allows removing single bad level in a levels string)
;; check-level now reports the bad index. levels are parsed earlier.
;; each possible omission from the levels string/vec is tried until something works (or not)

;; https://stackoverflow.com/questions/1394991/clojure-remove-item-from-vector-at-a-specified-location
(defn vec-remove
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn check-level-with-dampener [level]
  (let [[is-safe? _] (check-level level)]
    (if is-safe?
      true
      ;; Not safe, brute force all possible removals
      (loop [index 0]
        (if (>= index (count level))
          ;; No removal worked
          false
          (let [without-level (vec-remove index level)
                [is-safe-without-level? _] (check-level without-level)]
            (if is-safe-without-level?
              true
              (recur (inc index)))))))))

(defn safe-reports-amount-with-dampener [input]
  (reduce
   (fn [safe-count level] (+ safe-count (if (check-level-with-dampener (parse-level level)) 1 0)))
   0
   (str/split-lines input)))

(println (safe-reports-amount-with-dampener (slurp "input.txt")))
