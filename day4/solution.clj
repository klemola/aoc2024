(require '[clojure.string :as str])

;; 2D matrix helpers

(defn create-matrix-from-string [s rows cols]
  (let [chars (vec (seq s))]
    {:data chars
     :rows rows
     :cols cols}))

(defn get-element [matrix x y]
  (let [{:keys [data cols]} matrix]
    (data (+ (* x cols) y))))

(defn translate-coords [coords dir]
  (mapv + coords dir))

;; Common

(defn letter-matrix [input]
  (let
   [input-as-one-line (str/replace input #"\n" "")
    input-lines (str/split-lines input)
    rows (count input-lines)
    cols (count (first input-lines))]
    (create-matrix-from-string input-as-one-line rows cols)))

;; Part one (find all instances of XMAS, backwards as well)

(def directions
  [[0 1] [1 0] [1 1] [1 -1] [0 -1] [-1 0] [-1 -1] [-1 1]])

(defn find-string-in-dir [matrix match-string coords dir]
  (loop [expected-seq match-string
         [x y] coords]
    (cond
      ;; found a match
      (empty? expected-seq) [x y]
      ;; out of bounds
      (or (< y 0) (>= y (:rows matrix)) (< x 0) (>= x (:cols matrix))) nil
      ;; failure
      (not= (first expected-seq) (get-element matrix x y)) nil
      :else (recur (rest expected-seq)
                   (translate-coords [x y] dir)))))

(defn find-x-xmas-matches [matrix]
  (let [rows (:rows matrix)
        cols (:cols matrix)]
    (for [x (range cols)
          y (range rows)
          dir directions
          :let [coords [x y]
                match (if (= (get-element matrix x y) \X)
                        (find-string-in-dir matrix "MAS" (translate-coords coords dir) dir)
                        nil)]
          :when match]
      match)))

(->>
 (slurp "input.txt")
 letter-matrix
 find-x-xmas-matches
 count
 println)

;; Part two (find all instances of X-MAS, a X of S's, M's with A in the center)

(defn searches [origin matrix]
  [;; go top-left to bottom-right, forwards
   (find-string-in-dir matrix "MAS" (translate-coords origin [-1 -1]) [1 1])
   ;; go top-left to bottom-right, backwards
   (find-string-in-dir matrix "SAM" (translate-coords origin [-1 -1]) [1 1])
   ;; go top-right to left, forwards
   (find-string-in-dir matrix "MAS" (translate-coords origin [1 -1]) [-1 1])
   ;; go top-right to left, backwards
   (find-string-in-dir matrix "SAM" (translate-coords origin [1 -1]) [-1 1])])

(defn find-a-x-mas-matches [matrix]
  (let [rows (:rows matrix)
        cols (:cols matrix)]
    (for [x (range cols)
          y (range rows)
          :let [coords [x y]
                match (if (= (get-element matrix x y) \A)
                        (->>
                         (searches coords matrix)
                         (filter some?)
                         (count)
                         (= 2))
                        nil)]
          :when match]
      match)))

(->>
 (slurp "input.txt")
 letter-matrix
 find-a-x-mas-matches
 count
 println)
