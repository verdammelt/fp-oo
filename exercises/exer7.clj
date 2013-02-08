;; #1
(-> [1] first inc list)
;; #2
(-> [1] first inc (* 3) list)
;; #3
;(-> 3 (* 2) inc) ; replace (* 2) with (fn [n] (* 2))
(-> 3 ((fn [n] (* 2 n))) inc)
;;
; (+ (* (+ 1 2) 3) 4) switch to using ->
(-> 1 (+ 2) (* 3) (+ 4))

;;; provided code
(use 'clojure.set) ;; Not actually used here, but will be in exercises.


(def answer-annotations
     (fn [courses registrants-courses]
       (let [checking-set (set registrants-courses)]
         (map (fn [course]
                (assoc course
                       :spaces-left (- (:limit course)
                                       (:registered course))
                       :already-in? (contains? checking-set
                                               (:course-name course))))
              courses))))

(def domain-annotations
     (fn [courses]
       (map (fn [course]
              (assoc course
                :empty? (zero? (:registered course))
                :full? (zero? (:spaces-left course))))
            courses)))

(def note-unavailability
     (fn [courses instructor-count]
       (let [out-of-instructors?
             (= instructor-count
                (count (filter (fn [course] (not (:empty? course)))
                               courses)))]
         (map (fn [course]
                (assoc course
                       :unavailable? (or (:full? course)
                                         (and out-of-instructors?
                                              (:empty? course)))))
              courses))))

(def annotate
     (fn [courses registrants-courses instructor-count]
       (-> courses
           (answer-annotations registrants-courses)
           domain-annotations
           (note-unavailability instructor-count))))

(def separate
     (fn [pred sequence]
       [(filter pred sequence) (remove pred sequence)]))


(def visible-courses
     (fn [courses]
       (let [[guaranteed possibles] (separate :already-in? courses)]
         (concat guaranteed (remove :unavailable? possibles)))))

(def final-shape
     (fn [courses]
       (let [desired-keys [:course-name :morning? :registered :spaces-left :already-in?]]
         (map (fn [course]
                (select-keys course desired-keys))
              courses))))

(def half-day-solution
     (fn [courses registrants-courses instructor-count]
       (-> courses
           (annotate registrants-courses instructor-count)
           visible-courses
           ((fn [courses] (sort-by :course-name courses)))
           final-shape)))

(def solution
     (fn [courses registrants-courses instructor-count]
       (map (fn [courses]
              (half-day-solution courses registrants-courses instructor-count))
            (separate :morning? courses))))


;;;; 7.9 exercises
;;; #1 managers can't take afternoon courses
;; add :manager true to the registrant
(def registrant {:manager true, :classes ["foo", "bar"]})

;; parameter name change
(def solution
  (fn [courses registrant instructor-count]
    (map (fn [courses]
           (half-day-solution courses registrant instructor-count))
         (separate :morning? courses))))

;; parameter name change
(def half-day-solution
     (fn [courses registrant instructor-count]
       (-> courses
           (annotate registrant instructor-count)
           visible-courses
           ((fn [courses] (sort-by :course-name courses)))
           final-shape)))

;; parameter name change
;; splitting parameter and passing new manager? flag into note-unavailability
(def annotate
     (fn [courses registrant instructor-count]
       (-> courses
           (answer-annotations (:classes registrant))
           domain-annotations
           (note-unavailability instructor-count (:manager? registrant)))))

;; new parameter
;; using new parameter in definition of :unavailable?
(def note-unavailability
     (fn [courses instructor-count manager?]
       (let [out-of-instructors?
             (= instructor-count
                (count (filter (fn [course] (not (:empty? course)))
                               courses)))]
         (map (fn [course]
                (assoc course
                       :unavailable? (or (:full? course)
                                         (and out-of-instructors?
                                              (:empty? course))
                                         (and manager?
                                              (not (:morning? course))))))
              courses))))

;;; #2 - add prerequisite idea
;; add more data to registrant. Now has a :past-classes list of class names
;; add more data to courses Now has a :prereqs list of class names

;; add new parameter to note-unavailability
(def annotate
     (fn [courses registrant instructor-count]
       (-> courses
           (answer-annotations (:classes registrant))
           domain-annotations
           (note-unavailability instructor-count (:manager? registrant) (:past-classes registrant)))))

;; new parameter
;; use that in :unavailable? calculation
(def note-unavailability
     (fn [courses instructor-count manager?]
       (let [out-of-instructors?
             (= instructor-count
                (count (filter (fn [course] (not (:empty? course)))
                               courses)))]
         (map (fn [course]
                (assoc course
                       :unavailable? (or (:full? course)
                                         (and out-of-instructors?
                                              (:empty? course))
                                         (and manager?
                                              (not (:morning? course)))
                                         (and (superset? past-classes
                                                         (:prereqs course))))))
              courses))))
