;; exer 1
;; rewrite (map (fn [n] (+ 2 n)) [1 2 3]) in 'point-free' style
((partial map (partial + 2)) [1 2 3])

;; exer 2
;; juxt is a weird function. It takes N functions and returs a vector
;; of length N where the first element is the result of the 1st
;; function, the 2nd element is the result of the 2nd function etc.
((juxt empty? reverse count) [:a :b :c]) ; => [false (:c :b :a) 3]

;; use juxt to redefine 
(def separate 
  (fn [pred sequence] 
    [(filter pred sequence) (remove pred sequence)]))
(def my-separate
  (fn [pred sequence]
    ((juxt (partial filter pred) 
           (partial remove pred)) 
     sequence)))

;; NOTE: this could be so much simpler:
;; (def my-separate (juxt filter remove))  (!!!)

;; exercise 3
;; consider this code
(def myfun
  (let [x 3]
    (fn [] x)))

;; what does x evaluate to and why?
;; undefined since x is not defined except in that let
;; what does (myfun) evaluate to and why?
;; (myfun)=> 3 because x is 3 when the function is defined.


;; exer 4
;; how to do 'let' if let was undefined
(def my-other-fun
  ((fn [x]
    (fn [] x)) 3))

;; exer 5
(def my-atom (atom 0))
(swap! my-atom inc)
(deref my-atom) ; => 1
; how to set my-atom to 33 unconditionally
(swap! my-atom (fn [x] 33))

;; exer 6
;; write a function always that takes a value and retunrns a function
;; which always returns that value - like i could have used above
(def always 
  (fn [x]
    (fn [& args] x)))

;; exer 7
;; write a method which takes a vector of integers [v1, v2, v3...vn] and does the
;; following: 
;; (+ (* 1 v1) (* 2 v2) (* 3 v3)...(* n vn))
(def isbn-check-sum
  (fn [seq]
    (reduce + 
            (map * 
                 (range 1 (inc (count seq))) 
                 seq))))

;; exer 8
;; checking validity of isbn (check-sum must be divisible by 11)
;; (use the following function and strings for the exercise)
(def reversed-digits
     (fn [string]
       (map (fn [digit-char]
              (-> digit-char str Integer.))
            (reverse string))))
(def isbn-strings ["0131774115" "0977716614" "1934356190"])

(def isbn? 
  (fn [num]
    (let [digits (reversed-digits num)]
      (= 0 (rem (isbn-check-sum digits) 11)))))

;; exer 9
;; upc checking is similar - odd positions multiply by 1 even by 3 and
;; the checksum must have zero remainder with 10
(def upc-check-sum
  (fn [seq]
    (reduce + 
            (map (fn [position digit]
                     (* digit (if (odd? position) 1 3)))
                 (range 1 (inc (count seq))) 
                 seq))))

(def upc-strings ["074182265830" "731124100023" "722252601404"])

(def upc?
  (fn [str]
    (let [digits (reversed-digits str)]
      (zero? (rem (upc-check-sum digits) 10)))))

;; exer 10
;; generalize the isdn? and upc? functions (and their check-sums) to a
;; function called number-checker
(def number-checker 
  (fn [check-summer modulo]
    (fn [str]
      (let [digits (reversed-digits str)]
        (zero? 
         (rem 
          (reduce + 
                  (map check-summer 
                       (range 1 (inc (count digits))) 
                       digits))
          modulo))))))

(def isbn? (number-checker (fn [p d] (* d p)) 11))
(def upc? (number-checker (fn [p d] (* d (if (odd? p) 1 3))) 10))
