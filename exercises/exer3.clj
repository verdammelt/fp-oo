;; provided source
(def Point
  (fn [x y]
    {:x x,
     :y y,
     :__class_symbol__ 'Point}))

(def x :x)
(def y :y)
(def class-of :__class_symbol__)

(def shift (fn [this xinc yinc]
             (Point (+ (x this) xinc) (+ (y this) yinc))))

(def Triangle
  (fn [x y z]
    {:p1 x,
     :p2 y,
     :p3 z,
     :__class_symbol__ 'Triangle}))
(def Triangle
     (fn [point1 point2 point3]
       {:point1 point1, :point2 point2, :point3 point3
        :__class_symbol__ 'Triangle}))


(def right-triangle (Triangle (Point 0 0)
                              (Point 0 1)
                              (Point 1 0)))

(def equal-right-triangle (Triangle (Point 0 0)
                                    (Point 0 1)
                                    (Point 1 0)))

(def different-triangle (Triangle (Point 0 0)
                                  (Point 0 10)
                                  (Point 10 0)))

;; the exercises
(def add 
  (fn [p1 p2]
    ;; (Point (+ (x p1) (x p2))
    ;;        (+ (y p1) (y p2)))
    (shift p1 (x p2) (y p2))
    ))

(def test-add
  (fn []
    (assert (= (add (Point 1 2) (Point 3 4)) (Point 4 6)))))

(def make (fn [klass & args] (apply klass args)))

;; thought this needed to be more complicated - but not? i guess that
;; make sense.
(def equal-triangles? = 
  ;; (fn [t1 t2]
  ;;   (and (apply = (map :point1 [t1 t2]))
  ;;        (apply = (map :point2 [t1 t2]))
  ;;        (apply = (map :point3 [t1 t2]))))
    )

(def test-equal-triangles 
  (fn []
    (assert (equal-triangles? right-triangle right-triangle))
    (assert (equal-triangles? right-triangle equal-right-triangle))
    (assert (not (equal-triangles? right-triangle different-triangle)))))

(def valid-triangle
  (fn [p1, p2, p3]
    (= (count (distinct [p1, p2, p3])) 3)))





