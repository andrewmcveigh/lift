(ns lift.shorthand.impl
  (:refer-clojure :exclude [read])
  (:require
   [clojure.pprint :as pp]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.shorthand.util :refer :all]))

(defprotocol Base (base [_]))

(defprotocol Read (read [_]))

(defprotocol Show (show [_]))

(defprotocol ToSpec
  (to-form [_])
  (to-spec [_]))

(defmacro basetype
  {:style/indent [2 1]}
  [tagname fields & extends]
  (let [classname (symbol (str (namespace-munge *ns*) ".types." tagname))
        ex        (->> extends
                       (reduce (fn [[head & more :as init] x]
                                 (if (s/valid? (s/and simple-symbol?
                                                      #(re-matches #"^[A-Z]\w+$" (name %))) x)
                                   (conj init [x []])
                                   (conj more (update head 1 conj  x))))
                               ())
                       (into {}))
        pred      (predicate-name tagname)]
    `(do
       (declare ~pred)
       (deftype* ~(symbol (name (ns-name *ns*)) (name tagname))
         ~classname
         ~fields
         ;; ~(conj fields
         ;;        '^int ^:unsynchronized-mutable _hasheq
         ;;        '^int ^:unsynchronized-mutable _hash)

         :implements
         ~(vec
           (concat
            '[clojure.lang.IType
              clojure.lang.IHashEq
              lift.shorthand.impl.Base
              lift.shorthand.impl.Show]
            (when (seq fields)
              '[clojure.lang.ILookup])
            (map resolve-class (keys (dissoc ex 'Base 'Show)))))

         (equals
          [_# ~'other]
          (and (instance? ~tagname ~'other)
               ~@(map (fn [n] `(= (. ~'other ~(symbol (str \- n))) ~n))
                      fields)))
         (hasheq
          [_#]
          (int (bit-xor ~(hash tagname)
                        (clojure.lang.APersistentMap/mapHasheq
                         ~(zipmap (map #(list 'quote %) fields) fields))))
          #_(let [hq# ~'_hasheq]
            (if (zero? hq#)
              (let [hq'# (int (bit-xor ~(hash tagname)
                                       (clojure.lang.APersistentMap/mapHasheq
                                        ~(zipmap (map #(list 'quote %) fields) fields))))]
                (set! ~'_hasheq hq'#)
                hq'#)
              hq#)))
         (hashCode
          [_#]
          (clojure.lang.APersistentMap/mapHasheq
           ~(zipmap (map #(list 'quote %) fields) fields))
          #_(let [hq# ~'_hash]
            (if (zero? hq#)
              (let [hq'# (clojure.lang.APersistentMap/mapHasheq
                          ~(zipmap (map #(list 'quote %) fields) fields))]
                (set! ~'_hash hq'#)
                hq'#)
              hq#)))

         ~@(when (seq fields)
             `[(valAt
                [_# k#]
                (case k#
                  ~@(apply concat (map (fn [j] [(keyword j) j]) fields))
                  nil))
               (valAt
                [_# k# default#]
                (case k#
                  ~@(apply concat (map (fn [j] [(keyword j) j]) fields))
                  default#))])

         ~@(or (get ex 'Show)
              `[(lift.shorthand/show [_] (apply pr-str ~fields))])

         ~@(apply concat (vals (dissoc ex 'Show))))

       (defn ~tagname ~fields (new ~classname ~@fields; 0 0
                                   ))
       (defn ~pred [~'x]
         (instance? ~classname ~'x))

       (defmethod print-method ~classname [~'x ~'writer]
         (.write ~'writer (show ~'x)))

       ~classname)))

(defn basetype? [x] (instance? Base x))

(basetype Type []
  Show
  (show [_] "type?")
  ToSpec
  (to-form [_] 'type?)
  (to-spec [_] 'type-type?))

(basetype Unit []
  Show
  (show [_] "()"))

(basetype Var [var]
  ToSpec
  (to-form [_] var)
  (to-spec [_] any?))

(basetype Value [value])

(basetype Predicate [pred]
  ToSpec
  (to-form [_] pred)
  (to-spec [_] (deref (resolve pred))))

(basetype Param [type args]
  Show
  (show [_]
    (format "%s %s" (pr-str type) (string/join " " (map pr-str args)))))

(basetype Named [name type]
  Show
  (show [_]
    (format ":%s %s" name (pr-str type))))

(basetype Tuple [fields]
  Show
  (show [_] (format "(%s)" (string/join " * " (map pr-str fields))))
  ToSpec
  (to-form [_]
    `(s/tuple ~@(map to-form fields)))
  (to-spec [_]
    (s/tuple-impl (map to-form fields) (map to-spec fields))))

(basetype List [a]
  Show
  (show [_] (format "[%s]" (pr-str a)))
  ToSpec
  (to-form [_] `(s/coll-of ~(to-form a)))
  (to-spec [_]
    (s/every-impl (to-form a)
                  (to-spec a)
                  {::s/conform-all true
                   ::s/describe `(s/coll-of ~(to-form a))
                   ::s/cpred coll?})))

(basetype Expr [op args]
  Show
  (show [_] (format "(%s %s)"
                    (name op)
                    (string/join " " (map pr-str args)))))

(basetype Function [args return]
  Show
  (show [_]
    (format "(%s -> %s)"
            (string/join " -> " (map pr-str args))
            (pr-str return)))
  ToSpec
  (to-spec [x]
    `(s/fspec :args ~(->> args
                          (map-indexed (fn [i v] [(idx->key i) (to-spec v)]))
                          (apply concat)
                          (cons `s/cat))
              :ret ~(to-spec return))))

(basetype Dependent [op args bindings sig spec]
  Show
  (show [_]
    (let [val-types (remove (comp type-type? :type) (:args (:sig sig)))]
      (format "(%s ** %s %s)"
              (string/join ", " (map pr-str val-types))
              (pr-str op)
              (string/join " " (map pr-str args)))))
  ToSpec
  (to-spec [_] spec))

(basetype Sig [sig]
  Show
  (show [_] (str "#t/sig " (pr-str sig))))

;; n is the value of the relationship `count` of A and B
;; for generation we don't really care what that number is, unless it's fixed
;; a relationship only exists:
;; * In a compound type
;; * In a function - which is a sort of compound type
;; In a compound type, unless n is fixed, we only care that `count` is the same
;; or that the expression holds : (+ m n) = count A + count B = count C
;; In a function the same applies
;; If n is fixed, then we do care. We'd like the spec to properly represent that

;; (Dependent 'vect? '[n t] '{n count t unify-seq} (parse ::type '(nat-int? -> type? -> type?)) [])

(def parsers (atom []))

(defmulti construct first)

(defmacro defparser [t spec ctor]
  (if-let [sym (res t)]
    (let [key (keyword (namespace sym) (name sym))]
      `(do
         (swap! parsers conj [~key '~sym])
         (s/def ~sym ~spec)
         (defmethod construct ~key ~'[[_ ast]] (~ctor ~'ast))
         '~sym))
    (throw (Exception. (str "Could not resolve `t`: " (pr-str t))))))

(defparser Type #{'type?} (constantly (Type)))

(defparser Unit #{()} (constantly (Unit)))

(defparser Function
  (s/and seq?
         (s/cat ::type ::retype :more (s/+ (s/cat :_ #{'->} ::type ::retype))))
  (fn [x]
    (let [types (->> (map ::type (:more x))
                     (cons (::type x))
                     (map construct))]
     (Function (butlast types) (last types)))))

(defparser Named
  (s/cat :name (s/and simple-keyword? #(re-matches #"^[a-z]+$" (name %)))
         ::type ::retype)
  (fn [x]
    (Named (symbol (name (:name x))) (construct (::type x)))))

(defparser Param
  (s/alt :parens (s/and seq? (s/cat ::Predicate `Predicate :args (s/+ ::type)))
         :noparens (s/cat ::Predicate `Predicate :args (s/+ ::type)))
  (fn [[_ x]]
    (Param (Predicate (::Predicate x)) (map construct (:args x)))))

(defparser Var
  (s/and simple-symbol? #(re-matches #"^[a-z]+$" (name %)))
  (fn [x] (Var x)))

(defparser Predicate
  (s/and symbol? #(re-matches #".+\?$" (name %)))
  (fn [x] (Predicate x)))

(defparser Tuple
  (s/and seq? (s/cat :a* (s/+ (s/cat :a ::retype :* #{'*})) :a ::retype))
  (fn [x] (Tuple (map construct (conj (mapv :a (:a* x)) (:a x))))))

(defparser List
  (s/and vector? (s/cat :a ::type))
  (fn [x] (List (construct (:a x)))))

(defparser Expr
  (s/and seq? (s/cat :op (s/and symbol? #(not (s/valid? `Predicate %)))
                     :args (s/+ (s/or ::Var `Var ::Value `Value))))
  (fn [x] (Expr (:op x) (map construct (:args x)))))

(defparser Value
  (complement symbol? #_(partial s/valid? ::type))
  (fn [x] (Value x)))

;; (defparser Spec
;;   (s/cat :op symbol?
;;          :args (s/+ (s/or :spec-type ::type
;;                           :spec-var  ::type-var
;;                           :spec-any  any?))))

(defmacro build-type-parsers []
  (let [parsers' (->> @parsers
                      (map (fn [[k v]] [k (list 'quote v)]))
                      (apply concat))]
    `(do
       (s/def ::type ~(cons 's/or parsers'))
       (s/def ::retype ~(cons 's/alt parsers')))))

(defn conform [spec x]
  (do
    (build-type-parsers)
    (s/conform spec x)))

(defn explain [spec x]
  (do
    (build-type-parsers)
    (s/explain spec x)))

(defn pprint [spec x]
  (pp/pprint (conform spec x)))

(defn parse [spec x]
  (construct (conform spec x)))

(defn parse-type-signature [sig]
  (Sig (if (= (count sig) 1)
         (parse ::retype sig)
         (parse ::type sig))))
