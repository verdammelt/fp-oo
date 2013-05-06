;; building a zipper!

(def zseq
  (fn [tree] tree))

(def zdown (fn [zip] (first zip)))
(def znode (fn [zip] zip))

(assert (= (-> '(a b c) zseq zdown znode) 'a))
(assert (= (-> '((+ 1 2) 3 4) zseq zdown znode) '(+ 1 2)))
(assert (= (-> '((+ 1 2) 3 4) zseq zdown zdown znode) '+))

