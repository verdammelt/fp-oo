;; provided source
(def make
     (fn [class & args]
       (let [seeded {:__class_symbol__ (:__own_symbol__ class)}
             constructor  (:add-instance-values (:__instance_methods__ class))]
         (apply constructor seeded args))))

(def send-to
     (fn [instance message & args]
       (let [class (eval (:__class_symbol__ instance))
             method (message (:__instance_methods__ class))]
         (apply method instance args))))


(def Point
{
  :__own_symbol__ 'Point
  :__instance_methods__
  {
    :add-instance-values (fn [this x y]
                           (assoc this :x x :y y))
    :class :__class_symbol__
    :shift (fn [this xinc yinc]
             (make Point (+ (:x this) xinc)
                         (+ (:y this) yinc)))
    :add (fn [this other]
           (send-to this :shift (:x other)
                                (:y other)))
   :x :x
   :y :y
   }
 })


;; For exercise 4
(def Holder  
{
  :__own_symbol__ 'Holder
  :__instance_methods__
  {
    :add-instance-values (fn [this held]
                           (assoc this :held held))
  }
})

;; exercises

;; exercise #1
(def class-from-instance
  (fn [instance]
    (eval (:__class_symbol__ instance))))

(def method-from-message
  (fn [class message]
    (message (:__instance_methods__ class))))

(def apply-message-to
  (fn [class instance message args]
    (apply (method-from-message class message)
           instance args)))

(def make
     (fn [class & args]
       (let [seeded {:__class_symbol__ (:__own_symbol__ class)}]
         (apply-message-to class seeded :add-instance-values args))))

(def send-to
     (fn [instance message & args]
       (apply-message-to (class-from-instance instance) 
                         instance message args)))

(prn "chapter 5 exercise 1")
(prn (send-to (make Point 1 2) :add (make Point 3 4)))

(def Point
{
  :__own_symbol__ 'Point
  :__instance_methods__
  {
    :add-instance-values (fn [this x y]
                           (assoc this :x x :y y))
    :class-name :__class_symbol__
    :class (fn [this] (class-from-instance this))
    :shift (fn [this xinc yinc]
             (make Point (+ (:x this) xinc)
                         (+ (:y this) yinc)))
    :add (fn [this other]
           (send-to this :shift (:x other)
                                (:y other)))
   :x :x
   :y :y
   }
 })


(prn "chapter 5 exercise 2")
(prn (send-to (make Point 1 2) :class-name))
(prn (send-to (make Point 1 2) :class))


(prn "chapter 5 exercise 3")
(prn "make a test-point")
(def test-point (make Point 1 2))
(prn "redefine point")
(def Point
  {
   :__own_symbol__ 'Point
   :__instance_methods__
   {
    :origin (fn [this] (make Point 0 0))
    :add-instance-values (fn [this x y]
                           (assoc this :x x :y y))
    :class-name :__class_symbol__
    :class (fn [this] (eval (:__class_symbol__ this)))
    :shift (fn [this xinc yinc]
             (make Point (+ (:x this) xinc)
                   (+ (:y this) yinc)))
    :add (fn [this other]
           (send-to this :shift (:x other)
                    (:y other)))
    :x :x
    :y :y
    }
   })

(prn "send origin method to new point")
(prn (send-to (make Point 3 5) :origin))

(prn "test sending origin message to old point")
(prn (send-to test-point :origin))

(prn "chapter 5 exercise 4")
(def apply-message-to
  )
(def apply-message-to
  (fn [class instance message args]
    (let [method (or (method-from-message class message)
                     message)]
      (apply method instance args))))
(prn (send-to (make Holder "stuff") :held))
