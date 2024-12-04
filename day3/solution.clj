;; Part one, simple regexp match groups solution

(def regexp #"mul\((\d{1,3}),(\d{1,3})\)")

(defn add-mul-ops [input]
  (reduce +
          (map (fn [[_ a b]] (* (Integer/parseInt a)  (Integer/parseInt b)))
               (re-seq regexp input))))

(println (add-mul-ops (slurp "input.txt")))

;; Part two, conditional instructions

(defn process-instruction [[enabled? results] [_ do dont mul x y]]
  (cond
    do [true results]
    dont [false results]
    mul (if enabled?
          [enabled? (cons (* (Integer/parseInt x) (Integer/parseInt y)) results)]
          [enabled? results])
    :else [enabled? results]))

(defn add-conditional-mul-ops [input]
  (->> input
       (re-seq #"(do\(\))|(don't\(\))|(mul\((\d+),(\d+)\))")
       (reduce process-instruction [true []])
       second
       (apply +)))

(println (add-conditional-mul-ops (slurp "input.txt")))
