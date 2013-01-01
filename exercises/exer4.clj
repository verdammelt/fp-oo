(def make (fn [klass & args] (apply klass args)))

(def Point
  (fn [x y]
    { ;; initializing instance variables
     :x x,
     :y y

     ;; Metadata
     :__class_symbol__ 'Point
     :__methods__ {
                   :class :__class_symbol__
                   :x :x
                   :y :y
                   :shift (fn [this xinc yinc]
                            (make Point 
                                  (+ (send-to this :x) xinc)
                                  (+ (send-to this :y) yinc)))
                   :add (fn [this other-point]
                          (send-to this :shift 
                                   (send-to other-point :x)
                                   (send-to other-point :y)))
                   }
     }))

(def send-to
  (fn [object message & args]
    (apply (message (:__methods__ object)) object args)))

(prn "exercises chapter 4")
(def test-point (make Point 1 2))
(prn (send-to test-point :x))
(prn (send-to test-point :y))
(prn (send-to test-point :shift 10 20))
(prn (send-to test-point :add (make Point 3 4)))
