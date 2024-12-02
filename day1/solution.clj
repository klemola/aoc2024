(require '[clojure.string :as str])

;; Part one (sum up abs distances between sorted pairs)
;; 1. Split each line into two by the delimiter
;; 2. Add left and right parts into two collections line by line
;; 3. Sort the collections (ascending by integer value)
;; 4. Reduce the sorted collections with the distance calculation into a total integer

(defn collect-pair [left-right-vecs pair-line-str]
  (let [[left-values right-values] left-right-vecs
        [left right] (str/split pair-line-str #"   ")]
    [(cons (Integer/parseInt left) left-values) (cons (Integer/parseInt right) right-values)]))

(defn get-total [input]
  (let [[left-values right-values] (reduce collect-pair [[] []] (str/split-lines input))
        zipped (map vector (sort left-values) (sort right-values))]
    (reduce (fn [total pair] (+ total (abs (reduce - pair)))) 0  zipped)))

(print (get-total (slurp "input.txt")))
