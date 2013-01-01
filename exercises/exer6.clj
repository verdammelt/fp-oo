(load-file "sources/java.clj")

(def RedPoint
  {
   :__own_symbol__ 'RedPoint
   :__superclass_symbol__ 'Point
   })

(prn "chapter 6 exercise #1")
(def factorial
  (fn [n]
    (if (= n 1)
      1
      (* n
         (factorial (dec n))))))
(prn (factorial 5))
(prn (factorial 10))

(prn "chapter 6 exercise #2")
(def factorial-1
  (fn [n acc]
    (if (= n 1) acc
        (recur (dec n)
               (* n acc)))))

(def factorial
  (fn [n] (factorial-1 n 1)))
(prn (factorial 5))
(prn (factorial 10))

(prn "chapter 6 exercise #3")
(def add-numbers-1
  (fn [numbers acc]
    (if (empty? numbers) acc
        (recur (rest numbers)
               (+ (first numbers) acc)))))
(def add-numbers
  (fn [& numbers]
       (add-numbers-1 numbers 0)))

(prn (add-numbers 1 2 3 4))

(prn "chapter 6 exercise #4")
(def recursive-function
  (fn [fn numbers acc]
    (if (empty? numbers) acc
        (recur fn (rest numbers)
               (fn (first numbers) acc)))))
(def multiply-numbers
  (fn [& numbers]
    (recursive-function * numbers 1)))
(def add-numbers
  (fn [& numbers]
    (recursive-function + numbers 0)))

(prn (multiply-numbers 1 2 3 4))
(prn (add-numbers 1 2 3 4))

(prn "chapter 6 exercise 5")
(prn (recursive-function 
            (fn [new acc]
              (assoc acc new 0)) 
            [:a :b :c] 
            {}))
(prn (recursive-function
      (fn [new acc]
        (assoc acc new (count acc)))
      [:a :b :c]
      {}))

(prn "chapter 6 exercise 6")
(prn (reduce + 0 [1 2 3 4]))
(prn (reduce * 1 [1 2 3 4]))
(prn (reduce (fn [acc new] (assoc acc new 0)) {} [:a :b :c]))
(prn (reduce (fn [acc new] (assoc acc new (count acc))) {} [:a :b :c]))


