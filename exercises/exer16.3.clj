(use 'clojure.algo.monads)

(def function-monad
  (monad [m-result
          (fn [binding-value]
            (fn [] binding-value))

          m-bind
          (fn [monadic-value monadic-continuation]
            (let [binding-value (monadic-value)]
              (monadic-continuation binding-value)))]))

(def calculation
  (with-monad function-monad
    (let [frozen-step m-result]
      (domonad [a (frozen-step 8)
                b (frozen-step (+ a 88))]
               (+ a b)))))

;;;; Charging monad

(def charging-monad
  (monad [m-result 
          (fn [result]
            (fn [charge]
              {:charge charge, :result result}))

          m-bind
          (fn [monadic-value monadic-continuation]
            (fn [charge]
              (let [enclosed-map (monadic-value :any-old-value)
                    binding-value (:result enclosed-map)]
                ( (monadic-continuation binding-value)
                  (inc charge)))))]))

;; exercise #1
;; changing line the setting of enclosed-map to (monadic-value :blahblah)
;; work just fine because the charge member of the returned map is never used
;; (monadic-value x) creates a map with charge x and result = frozen value
;; but we never use the charge of it.

(def run-and-charge
  (with-monad charging-monad
    (let [frozen-step m-result]
      (domonad [a (frozen-step 8)
                b (frozen-step (+ a 88))]
               (+ a b)))))

(prn (run-and-charge 3))

;; exercise 2
;; run this code and be amazed!
(use '[clojure.pprint :only [cl-format]])

(def verbose-charging-monad
  (monad [m-result 
          (fn [result]
            (cl-format true "Freezing ~A.~%" result)
            (fn [charge]
              (cl-format true "Unfrozen calculation gets charge ~A.~%" charge)
              (cl-format true "... The frozen calculation result was ~A.~%" result)
              {:charge charge, :result result}))

          m-bind
          (fn [monadic-value monadic-continuation]
            (cl-format true "Making a decision.~%")
            (fn [charge]
              (let [enclosed-map (monadic-value charge)
                    binding-value (:result enclosed-map)]
                (cl-format true "Calling continuation with ~A~%" binding-value)
                (cl-format true "... The charge to increment is ~A~%", charge)
                ( (monadic-continuation binding-value)
                  (inc charge)))))]))

(println "==========")
(println "Defining run-and-charge.")

(def run-and-charge-and-speak
  (with-monad verbose-charging-monad
    (let [frozen-step m-result]
      (domonad [a (frozen-step 8)
                b (frozen-step (+ a 88))]
               (+ a b)))))

(println "-----------")
(println "Running run-and-charge.")
(run-and-charge-and-speak 3)

;; exercise 3
;; move the inc into the m-result
;; this causes it to get called too few times.
;; using the new value in the m-bind however causes it to be called too often.
(def verbose-charging-inc-monad
  (monad [m-result 
          (fn [result]
            (cl-format true "Freezing ~A.~%" result)
            (fn [charge]
              (cl-format true "Unfrozen calculation gets charge ~A.~%" charge)
              (cl-format true "... The frozen calculation result was ~A.~%" result)
              {:charge (inc charge), :result result}))

          m-bind
          (fn [monadic-value monadic-continuation]
            (cl-format true "Making a decision.~%")
            (fn [charge]
              (let [enclosed-map (monadic-value charge)
                    binding-value (:result enclosed-map)]
                (cl-format true "Calling continuation with ~A~%" binding-value)
                (cl-format true "... The charge was ~A~%", charge)
                ( (monadic-continuation binding-value) charge))))]))

(println "==========")
(println "Defining new run-and-charge.")

(def run-and-charge-and-speak
  (with-monad verbose-charging-inc-monad
    (let [frozen-step m-result]
      (domonad [a (frozen-step 8)
                b (frozen-step (+ a 88))]
               (+ a b)))))

(println "-----------")
(println "Running new run-and-charge.")
(run-and-charge-and-speak 3)
