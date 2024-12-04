;; Part one, simple regexp match groups solution

(def regexp #"mul\((\d{1,3}),(\d{1,3})\)")

(defn add-mul-ops [input]
  (reduce +
          (map (fn [[_ a b]] (* (Integer/parseInt a)  (Integer/parseInt b)))
               (re-seq regexp input))))

(println (add-mul-ops (slurp "input.txt")))
