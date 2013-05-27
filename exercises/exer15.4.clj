(use 'clojure.algo.monads)

;; the decider gets the ENTIRE value of the step (i.e. sequence)
;; that is why we need to wrap the monadic-continuation where not
;; just look for nil as the step-value
(def maybe-sequence-monad-decider
  (fn [step-value monadic-continuation]
    (let [maybe-ified-continuation
          (fn [binding-value]
            (if (nil? binding-value)
              (maybe-sequence-monad-monadifier binding-value)
              (monadic-continuation binding-value)))]
      (mapcat maybe-sequence-monad-monadifier step-value))
    ))

(def maybe-sequence-monad-monadifier list)

(def maybe-sequence-monad
     (monad [m-result maybe-sequence-monad-monadifier
             m-bind maybe-sequence-monad-decider]))

(prn
 (with-monad maybe-sequence-monad
   (domonad [a [1 2 3]
             b [-1 1]]
            (* a b))))

(prn 
 (with-monad maybe-sequence-monad
   (domonad [a [1 nil 3]
             b [-1 1]]
            (* a b))))
