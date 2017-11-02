(ns lift.type.core
  (:refer-clojure :exclude [class defn type])
  (:require
   [clojure.spec.alpha :as s]
   [lift.type.check :as check]
   [lift.type.parse :as parse]
   [lift.type.spec :as spec]
   [lift.type.syntax :as syn]
   [lift.type.type :as t]
   [lift.type.util :as u]
   [lift.type.substitution :as sub]))

(alias 'c 'clojure.core)

(defmacro tdef
  {:style/indent :defn}
  [sym & sig]
  `(let [sym# (u/resolve-sym '~sym)
         sig# (parse/type-signature '~sig)]
     (swap! t/expr-env assoc sym# (t/->Scheme (t/free sig#) sig#))
     sig#))

(defmacro data
  {:style/indent :defn}
  [& decl]
  `(parse/data ~@decl))

(defmacro deflit [type parser]
  (let [litmap (assoc @syn/lits (keyword (name type)) [parser type])
        ormap  (map (fn [[k [s]]] [k s]) litmap)
        summap (map (fn [[_ [_ t]]] [(symbol (str \L (name t))) t]) litmap)]
    `(do
       (reset! syn/lits '~litmap)
       (s/def ::syn/literal
         (s/or ~@(apply concat ormap)))
       (data ~'Lit ~'= ~@(apply concat (interpose '[|] summap))))))

(c/defn -check [expr]
  (->> expr
       (syn/parse)
       (check/infer {:ctx (t/map->Env @t/expr-env)})
       (second)))

(defmacro check [expr]
  `(-check '~expr))

(defmacro defn [name args expr]
  (let [inferred (-check (list 'fn args expr))
        declared (:t (get @t/expr-env (u/resolve-sym name)))]
    (assert (check/unify inferred declared))
    `(c/defn ~name ~args ~expr)))

(defmacro T [type-name]
  (get @t/type-env type-name))

(defmacro const [name parser]
  `(let [t# (t/Const '~name)]
     (swap! t/type-env assoc '~name t#)
     (deflit ~name ~parser)
     t#))

(c/defn type [x]
  (or (some-> x meta :type)
      (let [[t n] (syn/parse x)]
        (when (= ::syn/Lit t) (t/Const n)))))

(c/defn constraint-form [[_ {:keys [type-name args]}]]
  `(->> '~args
        (map parse/construct)
        (t/Constraint '~type-name)))

(defmacro class
  {:style/indent [:defn :defn]}
  [& decl]
  (let [{:keys [pre class sigs impls]} (s/conform ::spec/class decl)
        class' (second class)
        t (:type-name class')
        constrained (map parse/construct (:args class'))
        constraint (t/Constraint t constrained)
        constraint' (constraint-form class)
        precondition (constraint-form (:constraint pre))]
    `(do
       (swap! t/type-env assoc '~t {:constraint ~constraint'
                                    :precondition ~precondition})
       ~@(map
          (fn [{:keys [f sig]}]
            (let [f (u/ns-qualify f)
                  sig `(t/Constrained ~constraint' (parse/construct '~sig))]
              `(swap! t/expr-env assoc '~f (t/->Scheme (t/free ~sig) ~sig))))
          sigs)
       ~@(mapcat
          (fn [{:keys [f sig]}]
            (let [sig (parse/construct sig)
                  arglist (map vector (t/arglist sig) u/vars)]
              ;; TODO: ^^ is this always an arrow sig?
              `((ns-unmap *ns* '~f)
                (def ~f nil)
                (defmulti
                  ~f
                  (fn ~(mapv second arglist)
                    ~(->> arglist
                          (filter (comp #{(first constrained)} first))
                          (mapv (fn [[_ x]] `(type ~x)))))))))
          sigs)
       ~@(map (fn [{:keys [f args expr]}]
                `(defmethod ~f :default ~args ~expr))
              ;; TODO: ^^ needs type checking
              impls)
       ~constraint)))

(c/defn declare-instance [constraint instance]
  (swap! t/type-env
         update-in
         [constraint :instances]
         (fnil conj #{})
         instance))

(defmacro instance
  {:style/indent [2 1]}
  [constraint t & decl]
  (let [conformed (s/conform (s/coll-of ::spec/impl) decl)
        inst-form `(t/Constraint '~constraint [(T ~t)])
        classmeta (get @t/type-env constraint)]
    `(do
       (when-let [pre# '~(.tag (:precondition classmeta))]
         (let [constraint# (t/Constraint pre# [(T ~t)])]
           (assert (t/instance? constraint#)
                   (format "Instance did not meet precondition %s"
                           (pr-str constraint#)))))
       ~@(when-not (s/invalid? conformed)
           (mapv
            (fn [{:keys [f args expr]}]
              (let [f (u/resolve-sym f)
                    constrained (:t (get @t/expr-env f))
                    cvar (-> constrained .constraint t/free first)
                    subst (sub/singleton cvar t)
                    stype (sub/sub (.type constrained) subst)
                    dispatch (->> stype (t/arglist) (mapv (fn [t] `(T ~t))))]
                `(defmethod ~f ~dispatch ~args ~expr)))
            conformed))
       (declare-instance '~constraint ~inst-form)
       ~inst-form)))
