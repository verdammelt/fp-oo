;; starting code
(def prompt-and-read
     (fn []
       (print "> ")
       (.flush *out*)
       (.readLine
        (new java.io.BufferedReader *in*))))

;; exercise #1
(def inputs (repeatedly prompt-and-read))
(def one-character-line? (fn [line] (= (count line) 1)))
(def singles (filter one-character-line? inputs))
; (first singles)

;; Q: after loading some values into singles why doesn't redefining
;; inputs clear them out?
;; A: because singles itself is a lazy seq (uses filter) so you have
;; to redefine it (and inputs) to clear out the values.

;; exercise 2
(def ys-and-ns (filter (fn [s] (or (.startsWith s "y") (.startsWith s "n")))
                       inputs))

;; provided code for exercise #3
(def counted-sum
     (fn [number-count numbers]
       (apply +
              (take number-count
                    numbers))))

(def number-string?
     (fn [string]
       (try
         (Integer/parseInt string)
         true
       (catch NumberFormatException ignored
           false))))

(def to-integer
     (fn [string]
       (Integer/parseInt string)))

(def no-arg-counted-sum 
  (fn []
    (let [ numbers (map to-integer (filter number-string? inputs)) ]
      (apply + (take (first numbers) (rest numbers))))))

