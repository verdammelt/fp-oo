(load-file "sources/dynamic.clj")

(def ^:dynamic *active-message* nil)

;;; Constructing messages

(def find-containing-holder-symbol
     (fn [first-candidate message-name]
       (first (filter (fn [holder-symbol]
                        (message-name (held-methods holder-symbol)))
                      (reverse (lineage first-candidate))))))

(def fresh-active-message
     (fn [target name args]
       (let [holder-name (find-containing-holder-symbol (:__left_symbol__ target)
                                                        name)]
             (if holder-name
               {:name name, :holder-name holder-name, :args args, :target target}
               (fresh-active-message target
                                     :method-missing
                                     (vector name args))))))


(def using-method-above
     (fn [active-message]
       (let [symbol-above (method-holder-symbol-above (:holder-name active-message))
             holder-name (find-containing-holder-symbol symbol-above
                                                        (:name active-message))]
         (if holder-name
           (assoc active-message :holder-name holder-name)
           (throw (Error. (str "No superclass method `" (:name active-message)
                           "` above `" (:holder-name active-message)
                           "`.")))))))

;; Activating methods

(def method-to-run
     (fn [active-message]
       (get (held-methods (:holder-name active-message))
            (:name active-message))))

(def activate-method
     (fn [active-message]
       (binding [*active-message* active-message
                 this (:target active-message)]
         (apply (method-to-run active-message)
                (:args active-message)))))

;;; Public interface


(def send-to
     (fn [instance message-name & args]
       (activate-method (fresh-active-message instance message-name args))))

(def repeat-to-super
     (fn []
       (activate-method (using-method-above *active-message*))))
       
(def send-super
     (fn [& args]
       (let [with-replaced-args (assoc *active-message* :args args)]
         (activate-method (using-method-above with-replaced-args)))))

;; Klass
(install (method-holder 'Klass,
                        :left 'MetaKlass,
                        :up 'Module,
                        {
                         :new
                         (fn [& args]
                           (let [seeded {:__left_symbol__ (:__own_symbol__ this)}]
                             (apply send-to seeded :add-instance-values args)))

                         :to-string
                         (fn []
                           (str "class " (:__own_symbol__ this)))

                         :ancestors
                         (fn []
                           (remove invisible?
                                   (reverse (lineage (:__own_symbol__ this)))))
                         }))
                            
          
; exercise #1
(send-to Klass :new 
         'ActiveMessage 'Anything
         {
          :name (fn [] (:name this))
          :holder-name (fn [] this)
          :args (fn [] (:args this))
          :target (fn [] (:target this))

          :add-instance-values (fn [& map-args] 
                                 (merge this (apply hash-map map-args)))
          }
         {})

(def fresh-active-message
     (fn [target name args]
       (let [holder-name (find-containing-holder-symbol (:__left_symbol__ target)
                                                        name)]
             (if holder-name
               (binding [this (basic-object 'ActiveMessage)]
                 ((:add-instance-values ActiveMessage) 
                  [:name name, 
                   :holder-name holder-name, 
                   :args args, 
                   :target target])
                 )
               (fresh-active-message target
                                     :method-missing
                                     (vector name args))))))

(def fresh-active-message
  (fn [target name args]
    (let [holder-name (find-containing-holder-symbol 
                       (:__left_symbol__ target)
                       name)]
      (if holder-name
        {:name name, :holder-name holder-name, :args args, :target target}
        (fresh-active-message target
                              :method-missing
                              (vector name args))))))
(def send-to-Message-new  ;; Supposed to remind you of (send-to Message :new ...)
     (fn [target name args holder-name]
       (let [initializer (get (held-methods 'ActiveMessage) :add-instance-values)]
         (binding [this (basic-object 'ActiveMessage)]
           (initializer :name name
                        :holder-name holder-name
                        :args args
                        :target target)))))

(def fresh-active-message
     (fn [target name args]
       "Construct the message corresponding to the
      attempt to send the particular `name` to the
      `target` with the given `args`. If there is no
      matching method, the message becomes one that
      sends `:method-missing` to the target."
       (let [holder-name (find-containing-holder-symbol (:__left_symbol__ target)
                                                        name)]
             (if holder-name
               (send-to-Message-new target name args holder-name)
               (fresh-active-message target
                                     :method-missing
                                     (vector name args))))))




