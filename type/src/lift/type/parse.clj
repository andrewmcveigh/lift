(ns lift.type.parse
  (:require
   [clojure.spec.alpha :as s]
   [lift.type.spec :as spec]
   [lift.type.type :as t]
   [lift.type.util :as u]))

(defn ex-unknown-type [t]
  (throw
   (ex-info (format "Unknown Type %s" (pr-str t))
            {:type ::unknown-type
             :t t})))

(defmulti construct first)

(defmethod construct :unit  [[_ _]] (t/Unit))

(defmethod construct :type-name [[_ ast]]
  ;; TODO: when the parameter type is a value constructor, then a lookup in the
  ;; type env will not return.
  ;;
  (or (@t/type-env ast)
      (ex-unknown-type ast)))

(defmethod construct :type-var [[_ ast]] (t/Var ast))

(defmethod construct :arrow [[_ ast]]
  (->> (map :type (:more ast))
       (cons (:type ast))
       (map construct)
       (u/curry t/Arrow)))

(defmethod construct :parameterized [[_ [_ x]]]
  (let [tag (:type-name x)]
    (or (when-let [reg (get @t/type-env tag)]
          (when-let [ctor (cond (t/type-product? reg)
                                (partial t/Product tag)
                                (t/type-sum? reg)
                                (partial t/Sum tag)
                                (t/type-constraint? reg)
                                (partial t/Constraint tag))]
            (let [args (map construct (:args x))]
              (ctor args))))
        (ex-unknown-type tag))))

(defmethod construct :constrained [[_ {:keys [constraint sig]}]]
  (t/Constrained
   (construct [:parameterized (:constraint constraint)])
   (construct sig)))

(defmethod construct :non-constrained [[_ ast]]
  (construct ast))

(defmethod construct :sub-arrow [[_ ast]]
  (construct ast))

(defmethod construct :noparens [[_ ast]]
  (construct [:arrow ast]))

(defn type-signature [sig]
  (let [spec (if (= (count sig) 1) ::spec/resig ::spec/sig)
        conformed (s/conform spec sig)]
    (if (s/invalid? conformed)
      (do
        (s/explain spec sig)
        (throw (Exception. (format "Could not parse type signature %s" sig))))
      (construct (s/conform spec sig)))))


;;; `data` declaration parsing

(defn type-cons [type node]
  (let [[t n] (:type-cons node)]
    (case t
      :lit-type-cons `(~type '~n [])
      :par-type-cons `(~type '~(:type-name n)
                       (mapv t/Var '~(:type-params n))))))

(defn def-type-cons [type-cons]
  (swap! t/type-env assoc (.tag type-cons) type-cons))

(defn def-value-cons [sym signature]
  (swap! t/expr-env assoc
         (u/ns-qualify sym)
         (t/->Scheme (t/free signature) signature)))

(defn param-value-cons [[_ n] type-cons]
  (let [tag (:type-name n)
        arglist (vec (take (count (:args n)) u/vars))]
    `((def-value-cons '~tag
        (u/curry t/Arrow (concat (map construct '~(:args n)) [~type-cons])))
      (t/base ~tag ~arglist)
      (defn ~tag ~arglist
        (new ~(t/base-classname tag) ~@arglist {:type ~type-cons})))))

(defn value-cons [type [t n]]
  (case t
    :type-name `((ns-unmap *ns* '~n)
                 (def-value-cons '~n ~type)
                 (def ~n
                   (with-meta
                     (reify t/Show
                       (t/show [_#] ~(name n)))
                     {:type ~type})))
    :parameterized (param-value-cons n type)))

(defn sum-value-cons [node type-cons]
  (let [{:keys [type more]} (:sum-cons node)
        type-constructors (cons type (map :type more))]
    (mapcat (partial value-cons type-cons) type-constructors)))

(defn record-value-cons [n type-cons]
  (let [tag (:type-name n)
        arglist (mapv (comp symbol name) (keys (:recmap n)))]
    `((def-value-cons '~tag
        (->> [~type-cons]
             (concat (map construct '~(vals (:recmap n))))
             (u/curry t/Arrow)))
      (t/base ~tag ~arglist)
      (defn ~tag ~arglist
        (new ~(t/base-classname tag) ~@arglist {:type ~type-cons})))))

(defn data-cons [[t n]]
  (case t
    :product
    (let [type-cons (type-cons `t/Product n)]
      (concat
       `(do (def-type-cons ~type-cons))
       (param-value-cons (:value-cons n) type-cons)
       [type-cons]))
    :sum
    (let [type-cons (type-cons `t/Sum n)]
      (concat
       `(do (def-type-cons ~type-cons))
       (sum-value-cons n type-cons)
       [type-cons]))
    :record
    (let [type-cons (type-cons `t/Record n)]
      (concat
       `(do (def-type-cons ~type-cons))
       (record-value-cons (:rec-cons n) type-cons)
       [type-cons]))))

(defn data [decl]
  (let [ast (s/conform ::spec/data decl)]
    (if (s/invalid? ast)
      (do
        (s/explain ::spec/data decl)
        (throw (Exception. "Invalid Syntax")))
      ast)))
