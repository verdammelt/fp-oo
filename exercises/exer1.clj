
;; write a function that return the second item
(def second (fn [list] (nth list 1)))

;; two ways to write 'third'
(def third-1 (fn [list] (nth list 2)))
(def third-2 (fn [list] (first (rest (rest list)))))

(def add-squares
  (fn [& numbers]
    (apply + (map * numbers numbers))))

(def weird-factorial
  (fn [n] (apply * (range 1 (inc n)))))


;; exercise 8
(def prefix-of?
  (fn [x y]
    (= x (take (count x) y))))

(def tails
  (fn [x]
    (map drop
         (range (count x))
         (repeat (count x) x))))
