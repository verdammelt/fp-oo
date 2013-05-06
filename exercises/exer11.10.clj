;; building a zipper!

(def zseq
  (fn [tree]
    {:here tree
     :parents '()
     :lefts '()
     :rights '()}))

(def zroot 
  (fn [zip] 
    (let [parents (:parents zip)]
      (if (empty? parents) (znode zip)
          (zroot (zup zip))))))

(def znode 
  (fn [zip] (:here zip)))

(def zdown 
  (fn [zip] 
    (let [here (first (:here zip))]
      (and here 
           (let [new-parents (conj (:parents zip) zip)]
             (merge zip 
                    {:here here 
                     :parents new-parents 
                     :rights (map 
                              (fn [s] (merge zip {:here s :parents new-parents})) 
                            (rest (:here zip)))
                     :lefts '()
                   }))))))

(def zup 
  (fn [zip] 
    (let [parent (first (:parents zip))]
      (and parent
           (if (:changed zip)
             (merge parent
                    {:here (concat (reverse 
                                    (map znode (:lefts zip))) 
                                   (list (znode zip)) 
                                   (map znode (:rights zip)))
                     :changed (:changed zip)})
             parent)))))

(def zright
  (fn [zip] 
    (let [right (first (:rights zip))]
      (and right 
           (merge right
                  {:rights (rest (:rights zip))
                   :lefts (conj (:lefts zip) zip)
                   :changed (:changed zip)})))))

(def zleft
  (fn [zip] 
    (let [left (first (:lefts zip))]
      (and left 
           (merge left
                  {:rights (conj (:rights zip) zip)
                   :lefts (rest (:lefts zip))
                   :changed (:changed zip)})))))

(def zreplace
  (fn [zip new] 
    (merge zip {:here new
                :changed true})))

(assert (= (-> '(a b c) zseq znode) '(a b c)))
(assert (= (-> '(a b c) zseq zdown znode) 'a))
(assert (= (-> '(a b c) zseq zup) nil))
(assert (= (-> '() zseq zdown) nil))
(assert (= (-> '(a b c) zseq zdown zup znode) '(a b c)))
(assert (= (-> '((+ 1 2) 3 4) zseq zdown znode) '(+ 1 2)))
(assert (= (-> '((+ 1 2) 3 4) zseq zdown zdown znode) '+))
(assert (= (-> '(a) zseq zroot) '(a)))
(assert (= (-> '(((a)) b c) zseq zdown zdown zdown zroot) '(((a)) b c))) 

(assert (= (-> '(a b c) zseq zdown zright znode) 'b))
(assert (= (-> '(a b c) zseq zdown zright zright zleft znode) 'b)) 
(assert (= (-> '(a b c) zseq zdown zleft) nil)) 
(assert (= (-> '(a b c) zseq zdown zright zright zright) nil)) 
(assert (= (-> '(a b c) zseq zdown zup znode) '(a b c))) 

(assert (= (-> (zseq '(a b c)) zdown zright (zreplace 3) znode) 3))
(assert (= (-> (zseq '(a b c)) zdown zright (zreplace 3) zright zleft znode) 3))
(assert (= (-> (zseq '(a b c)) zdown zright (zreplace 3) zleft zright zright znode) 'c))

(assert (= (-> (zseq '(a b c)) zdown zright (zreplace 3) zup znode) '(a 3 c)))
(assert (= (-> (zseq '(a b c)) zdown zright (zreplace 3) zright (zreplace 4) zup znode) '(a 3 4)))
(assert (= (-> (zseq '(a)) zdown (zreplace 3) zup zup) nil))
(assert (= (-> (zseq '(a (b) c)) zdown zright zdown (zreplace 3) zroot) '(a (3) c)))
(assert (= (-> (zseq '(a (b) c)) zdown zright zdown (zreplace 3) zup zright (zreplace 4) zroot) '(a (3) 4)))
