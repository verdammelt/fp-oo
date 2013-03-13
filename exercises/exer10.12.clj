(use 'clojure.algo.monads)

;; write multiples which return all non-prime multiples of a number n less than 100
(def multiples 
  (fn [n]
    (range (* 2 n) 101 n)))

;; Use the Sequence monad or for (your choice!) to find all non-primes
;; less than 100. Duplicates are OK.  
;; Hint: You’ll need two steps and a body that just returns a value.
(def all-non-primes
  (with-monad sequence-m 
    (domonad [n (range 2 11)          ; only need to go up to SQRT(100)
              m (multiples n)]
             m)))

;; Use sets to calculate all the primes less than 100.
;; Hint: Here’s how you can tell if 6 is a non-prime less than 10:
;; user=> (#{4 6 8 9} 6)
;; 6
;; Hint: You’ll likely want to use remove.
(def primes-less-than-100
  (remove (set all-non-primes) (range 2 101)))




