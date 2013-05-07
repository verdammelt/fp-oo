;; can be used as a model:
;; (def rrange
;;   (fn [first past-end]
;;     (new clojure.lang.LazySeq
;;          (fn []
;;            (if (= first past-end)
;;              nil
;;              (cons first
;;                    (rrange (inc first) past-end)))))))

;; Exercise #1 - implement map
(def mymap 
  (fn [f ls]
    (new clojure.lang.LazySeq
         (fn []
           (if (empty? ls) nil
               (cons (f (first ls))
                     (mymap f (rest ls))))))))

;; exercise #2
(def myfilter
  (fn [p ls]
    (new clojure.lang.LazySeq
         (fn []
           (cond (empty? ls) nil

                 (p (first ls))
                 (cons (first ls)
                       (myfilter p (rest ls)))

                 :else
                 (myfilter p (rest ls)))))))

;; exercise 3
(def eager-filter
  (fn [p ls]
    (cond (empty? ls) '()
          
          (p (first ls))
          (cons (first ls) (eager-filter p (rest ls)))
          
          :else
          (eager-filter p (rest ls)))))
