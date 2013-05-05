(require '[clojure.zip :as zip])
(def test-seq '(- 3 (+ 6 (+ 3 4) (* 2 1) (/ 8 3))))

;; Exercise #3 - factor out tumult to remove duplication
;; (def tumult
;;      (fn [form]
;;        (letfn [(helper [zipper]
;;                        (cond (zip/end? zipper)
;;                              zipper
                             
;;                              (= (zip/node zipper) '+)
;;                              (-> zipper
;;                                  (zip/replace 'PLUS)
;;                                  zip/next
;;                                  helper)

;;                              (and (zip/branch? zipper)
;;                                   (= (-> zipper zip/down zip/node) '-))
;;                              (-> zipper
;;                                  (zip/append-child 55555)
;;                                  zip/next
;;                                  helper)

;;                              (and (zip/branch? zipper)
;;                                   (= (-> zipper zip/down zip/node) '*))
;;                              (-> zipper
;;                                  (zip/replace '(/ 1 (+ 3 (- 0 9999))))
;;                                  zip/next
;;                                  helper)

;;                              (= (zip/node zipper) '/)
;;                              (-> zipper
;;                                  zip/right
;;                                  zip/remove
;;                                  zip/right
;;                                  zip/remove
;;                                  (zip/insert-right (-> zipper zip/right zip/node))
;;                                  (zip/insert-right (-> zipper zip/right zip/right zip/node))
;;                                  zip/next
;;                                  helper)

;;                              :else 
;;                              (-> zipper zip/next helper)))]
;;        (-> form zip/seq-zip helper zip/root))))

(def handle-plus (fn [z] (-> z zip/down (zip/replace 'PLUS))))
(def handle-minus (fn [z] (-> z (zip/append-child 55555))))
(def handle-mult (fn [z] (-> z (zip/replace '(/ 1 (+ 3 (- 0 9999)))))))
(def handle-div (fn [z]
                  (-> z 
                      zip/down
                      zip/right
                      zip/remove
                      zip/right
                      zip/remove
                      (zip/insert-right (-> z zip/down zip/right zip/node))
                      (zip/insert-right (-> z zip/down zip/right zip/right zip/node)))))
(def tumult
     (fn [form]
       (letfn [(advancing [form] (-> (form) zip/next helper))
               (helper [zipper]
                 (cond (zip/end? zipper)
                       zipper
                       
                       (zip/branch? zipper)
                       (let [op-fn-map { '+ handle-plus,
                                        '- handle-minus,
                                        '* handle-mult,
                                        '/ handle-div}
                             fun (get op-fn-map 
                                     (-> zipper zip/down zip/node) 
                                     identity)]
                         (advancing (fn [] (fun zipper))))
                                                    
                       :else 
                       (-> zipper zip/next helper)))]
       (-> form zip/seq-zip helper zip/root))))


