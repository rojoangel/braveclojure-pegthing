(ns pegthing.core
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  "Is the number triangular? e.g. 1, 3, 6, 10, 15, etc"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri
  "The triangular number at the end of row n"
  [n]
  (last (take n tri)))

(defn row-num
  "Returns row number the position belongs to: pos 1 in row 1,
  positions 2 and 3 in row 2, etc"
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))

(defn connect-right
  [board max-pos pos]
  (let [neighbour (inc pos)
        destination (inc neighbour)]
    (if-not (or (triangular? neighbour) (triangular? pos))
      (connect board max-pos pos neighbour destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbour (+ row pos)
        destination (+ 1 row neighbour)]
    (connect board max-pos pos neighbour destination)))
