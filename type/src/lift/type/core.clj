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

(defmacro tdef [sym & sig]
  `(let [sym# (u/resolve-sym '~sym)
         sig# (parse/type-signature '~sig)]
     (swap! t/expr-env assoc sym# (t/->Scheme (t/free sig#) sig#))
     sig#))

(defmacro data
  {:style/indent :defn}
  [& decl]
  (parse/data-cons (parse/data decl)))

(c/defn -check [expr]
  (->> expr
       (syn/parse)
       (check/infer (check/map->Env @t/expr-env))
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
     (syn/deflit ~name ~parser)
     t#))

(c/defn type [x]
  (or (some-> x meta :type)
      (let [[t n] (syn/parse x)]
        (when (= ::syn/Lit t) (t/Const n)))))

(defmacro class
  {:style/indent [:defn :defn]}
  [& decl]
  (let [{:keys [class sigs impls]} (s/conform ::spec/class decl)
        class (second class)
        t (:type-name class)
        constrained (map parse/construct (:args class))
        constraint  (t/Constraint t constrained)
        constraint' `(->> '~(:args class)
                          (map parse/construct)
                          (t/Constraint '~t))]
    `(do
       (swap! t/type-env assoc '~t {:constraint ~constraint'})
       ~@(map
          (fn [{:keys [f sig]}]
            (let [f (u/ns-qualify f)
                  sig `(t/Constrained ~constraint'(parse/construct '~sig))]
              `(swap! t/expr-env assoc '~f (t/->Scheme (t/free ~sig) ~sig))))
          sigs)
       ~@(map (fn [{:keys [f sig]}]
                (let [sig (parse/construct sig)
                      arglist (map vector (t/arglist sig) u/vars)]
                  ;; TODO: ^^ is this always an arrow sig?
                  `(defmulti
                     ~f
                     (fn ~(mapv second arglist)
                       ~(->> arglist
                             (filter (comp #{(first constrained)} first))
                             (mapv (fn [[_ x]] `(type ~x))))))))
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
        inst-form `(t/Constraint '~constraint [(T ~t)])]
    `(do
       ~@(mapv
          (fn [{:keys [f args expr]}]
            (let [f (u/resolve-sym f)
                  constrained (:t (get @t/expr-env f))
                  cvar (-> constrained .constraint t/free first)
                  subst (sub/singleton cvar t)
                  stype (sub/sub (.type constrained) subst)
                  dispatch (->> stype (t/arglist) (mapv (fn [t] `(T ~t))))]
              `(defmethod ~f ~dispatch ~args ~expr)))
          conformed)
       (declare-instance '~constraint ~inst-form)
       ~inst-form)))
