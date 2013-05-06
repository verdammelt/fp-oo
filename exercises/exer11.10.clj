;; building a zipper!

(def zseq
  (fn [tree]
    {:here tree
     :parents '()}))

(def zroot 
  (fn [zip] 
    (let [parents (:parents zip)]
      (if (empty? parents) (znode zip)
          (znode (last (:parents zip)))))))

(def znode 
  (fn [zip] (:here zip)))

(def zdown 
  (fn [zip] 
    (let [here (first (:here zip))]
      (and here 
           (merge zip {:here here :parents (conj (:parents zip) zip)})))))

(def zup 
  (fn [zip] (first (:parents zip))))

(assert (= (-> '(a b c) zseq znode) '(a b c)))
(assert (= (-> '(a b c) zseq zdown znode) 'a))
(assert (= (-> '(a b c) zseq zup) nil))
(assert (= (-> '() zseq zdown) nil))
(assert (= (-> '(a b c) zseq zdown zup znode) '(a b c)))
(assert (= (-> '((+ 1 2) 3 4) zseq zdown znode) '(+ 1 2)))
(assert (= (-> '((+ 1 2) 3 4) zseq zdown zdown znode) '+))
(assert (= (-> '(a) zseq zroot) '(a)))
(assert (= (-> '(((a)) b c) zseq zdown zdown zdown zroot) '(((a)) b c))) 

