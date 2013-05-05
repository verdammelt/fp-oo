(require '[clojure.zip :as zip])

(def test-seq '(fn [a b] (concat [a] [b])))

(def all-vectors 
  (fn [s] 
    (letfn [(collect [z so-far]
              (cond (zip/end? z) 
                    so-far

                    (vector? (zip/node z))
                    (collect (zip/next z)
                             (cons (zip/node z) so-far))

                    :else
                    (collect (zip/next z) so-far)))]
      (reverse (collect (zip/seq-zip s) '())))))

(def first-vector
  (fn [s]
    (letfn [(find-first-vector [z]
              (cond (zip/end? z) nil
                    (vector? (zip/node z)) (zip/node z)
                    :else (find-first-vector (zip/next z))))]
      (find-first-vector (zip/seq-zip s)))))
