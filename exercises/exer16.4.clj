(use '[clojure.pprint :only [cl-format]])

(def state-monad
  (monad [m-result 
          (fn [result]
            (fn [state]
              {:state state, :result result}))

          m-bind
          (fn [monadic-value monadic-continuation]
            (fn [state]
              (let [enclosed-map (monadic-value state)
                    binding-value (:result enclosed-map)
                    new-state (:state enclosed-map)]
                (  (monadic-continuation binding-value) new-state))))]))

(def get-state
  (fn []
    (fn [state]
      {:state state, :result state})))

(def assign-state
  (fn [new-state]
    (fn [state]
      {:state new-state, :result state})))


(def calculation-with-initial-state
  (with-monad state-monad
    (domonad [original-state (assign-state 88)
              state (get-state)]
             (str "original state " original-state " was set to " state))))

(def mixer
  (with-monad state-monad
    (let [frozen-step m-result]
      (domonad [original (get-state)
                a (frozen-step (+ original 88))
                b (frozen-step (* a 2))
                _ (assign-state b)]
               [original a b]))))

;; exercise #4
;; write transform-state - takes a function as argument - it applies
;; that function to the current state to product the new state
(defn transform-state [f]
  (fn [state]
    {:state (f state), :result state}))

(def transform-state-example
  (domonad [b (transform-state inc)]
           b))

(prn (transform-state-example 1))

;; exercise #5
;; change the above monad to have its state be a map - all accessors
;; will take a map key and only access/change that item in the state.
;; the :state returned by each function is only the value of the key - 
;; not the whole state.
(def map-state-monad
  (monad [m-result 
          (fn [result]
            (cl-format true "Freezing ~A.~%" result)
            (fn [state]
              {:state state, :result result}))

          m-bind
          (fn [monadic-value monadic-continuation]
            (fn [state]
              (let [enclosed-map (monadic-value state)
                    binding-value (:result enclosed-map)
                    new-state (:state enclosed-map)]
                ((monadic-continuation binding-value) new-state))))]))

(def get-map-state
  (fn [key]
    (fn [state]
      {:state state, :result (key state)})))

(def assign-map-state
  (fn [key new-state]
    (fn [state]
      (let [old-state (key state)]
        {:state (assoc state key new-state) :result old-state}))))

(def transform-map-state
  (fn [key f]
    (fn [state]
      (let [old-state (key state)]
        {:state (assoc state key (f old-state)) :result old-state }))))

(def example
  (with-monad state-monad
       (domonad [a (get-map-state :a)
                 old-b (assign-map-state :b 3)
                 old-c (transform-map-state :c inc)]
          [a old-b old-c])))

(prn (example {:a 1 :b 2 :c 3}))

