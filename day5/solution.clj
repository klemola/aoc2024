(require '[clojure.string :as str])

(defn create-order-map [order-tuples]
  (reduce (fn [order-map [left right]]
            (update order-map left (fnil conj #{}) right)) {} order-tuples))

(defn is-before [a b order-map]
  (contains? (order-map a) b))

(defn middle-number [nums]
  (nth nums (quot (count nums) 2)))

(defn parse-input [input]
  (let [[order-str pages-str] (str/split input #"\n\n")
        order-tuples (map (fn [s] (mapv Integer/parseInt (str/split s #"\|"))) (str/split-lines order-str))
        order-map (create-order-map order-tuples)
        pages (->>
               pages-str
               str/split-lines
               (mapv (fn [nums-str] (mapv Integer/parseInt (str/split nums-str #",")))))]
    [order-map pages]))

;; Part one

(defn validate-page [order-map page]
  (loop [[current & rest] page]
    (cond (empty? rest) true
          (not (is-before current (first rest) order-map)) false
          :else (recur rest))))

(defn count-valid-pages [input]
  (let [[order-map, pages] (parse-input input)]
    (->>
     pages
     (filterv (partial validate-page order-map))
     (map middle-number)
     (apply +))))

(println (count-valid-pages (slurp "input.txt")))

;; Part two

(defn count-invalid-sorted-pages [input]
  (let [[order-map, pages] (parse-input input)]
    (->>
     pages
     (filter (complement (partial validate-page order-map)))
     (map (partial sort (fn [a b] (is-before b a order-map))))
     (map middle-number)
     (apply +))))

(println (count-invalid-sorted-pages (slurp "input.txt")))
