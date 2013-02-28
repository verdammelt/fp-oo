;; #1
;; transform this to continuation passing style
(let [a (concat '(a b c) '(d e f))
      b (count a)]
  (odd? b))

(-> (concat '(a b c) '(d e f))
    ((fn [l] (-> (count l)
                 ((fn [n] (odd? n)))))))

;; #2
;; transform the following to continuation passing style
(odd? (count (concat '(a b c) '(d e f))))

;; same as above - but not /necessarily/ the same. could go as far as.
(-> (list 'd 'e 'f)
    ((fn [x] (-> (concat (list 'a 'b 'c) x)
                 ((fn [l] (-> (count l)
                              ((fn [n] (odd? n))))))))))
;; #3
;; transform this to continuation passing style:
(-> 3
    (+ 2)
    inc)

(-> 3
    ((fn [n] (-> (+ n 2)
                 ((fn [m] (inc m)))))))


