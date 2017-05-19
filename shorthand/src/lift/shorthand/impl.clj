(ns lift.shorthand.impl
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.shorthand.util :refer :all]))

(defprotocol Base (base [_]))

(defprotocol Read (read [_]))

(defprotocol Show (show [_]))

(defprotocol ToSpec (to-spec [_]))

(defmacro basetype
  {:style/indent [2 1]}
  [tagname fields & extends]
  (let [classname (symbol (str (namespace-munge *ns*) ".types." tagname))
        ex        (apply array-map extends)]
    `(do
       (deftype* ~(symbol (name (ns-name *ns*)) (name tagname))
         ~classname
         ~(conj fields
                '^int ^:unsynchronized-mutable ___hasheq
                '^int ^:unsynchronized-mutable ___hash)

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
          (let [hq# ~'___hasheq]
            (if (zero? hq#)
              (let [hq'# (int (bit-xor ~(hash tagname)
                                       (clojure.lang.APersistentMap/mapHasheq
                                        ~(zipmap (map #(list 'quote %) fields) fields))))]
                (set! ~'___hasheq hq'#)
                hq'#)
              hq#)))
         (hashCode
          [_#]
          (let [hq# ~'___hash]
            (if (zero? hq#)
              (let [hq'# (clojure.lang.APersistentMap/mapHasheq
                          ~(zipmap (map #(list 'quote %) fields) fields))]
                (set! ~'___hash hq'#)
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

         ~(or (get ex 'Show)
              `(lift.shorthand/show [_] (apply pr-str ~fields)))

         ~@(vals (dissoc ex 'Show)))

       (defn ~tagname ~fields (new ~classname 0 0 ~@fields))
       (defn ~(predicate-name tagname) [~'x]
         (instance? ~classname ~'x))

       (defmethod print-method ~classname [~'x ~'writer]
         (.write ~'writer (show ~'x)))

       ~classname)))

(defn basetype? [x] (instance? Base x))

(basetype Unit []
  Show
  (show [_] "()"))

(basetype Var [var]
  ToSpec
  (to-spec [_] (list 'quote var)))

(basetype Predicate [pred]
  ToSpec
  (to-spec [_] pred))

(basetype Type [type args]
  Show
  (show [_]
    (format "%s %s" (pr-str type) (string/join " " (map pr-str args)))))

(basetype Tuple [fields]
  Show
  (show [_] (format "(%s)" (string/join " * " (map pr-str fields))))
  ToSpec
  (to-spec [_]
    `(s/tuple  ~@(map to-spec fields))))

(basetype List [a]
  Show
  (show [_] (format "(%s)" a))
  ToSpec
  (to-spec [_] `(s/coll-of ~(to-spec a))))

(basetype Vector [a]
  Show
  (show [_] (format "[%s]" a))
  ToSpec
  (to-spec [_] `(s/coll-of ~(to-spec a) :kind vector?)))

(basetype Expr [op args])

(basetype Function [args return]
  clojure.lang.ISeq
  (seq [_] (concat args [return]))
  Show
  (show [_]
    (format "(%s -> %s)"
            (string/join " -> " (map pr-str args))
            (pr-str return))))

(def parsers (atom {}))

(defmulti construct first)

(defmacro defparser [t spec ctor]
  (if-let [sym (res t)]
    (let [key (keyword (namespace sym) (name sym))]
      `(do
         (swap! parsers assoc ~key '~sym)
         (s/def ~sym ~spec)
         (defmethod construct ~key ~'[[_ ast]] (~ctor ~'ast))
         '~sym))
    (throw (Exception. (str "Could not resolve `t`: " (pr-str t))))))

(defparser Var
  (s/and simple-symbol? #(re-matches #"^[a-z]+$" (name %)))
  (fn [x] (Var x)))

(defparser Predicate
  (s/and symbol? #(re-matches #".+\?$" (name %)))
  (fn [x] (Predicate x)))

(defparser Type
  (s/cat :type ::predicate :args (s/+ ::type))
  (fn [x] (Type (:type x) (map construct (:args x)))))

(defparser Tuple
  (s/and seq? (s/cat :a* (s/* (s/cat :a ::type :* #{'*})) :a ::type))
  (fn [x] (Tuple (map construct (conj (mapv :a (:a* x)) (:a x))))))

(defparser List
  (s/and seq? (s/cat :a ::type))
  (fn [x] (List (construct (:a x)))))

(defparser Vector
  (s/and vector? (s/cat :a ::type))
  (fn [x] (Vector (construct (:a x)))))

;; (defparser Expr
;;   (s/and seq? (s/cat :op symbol? :args (s/+ (s/or ::Var `Var ::Value `Value)))))

;; (defparser Spec
;;   (s/cat :op symbol?
;;          :args (s/+ (s/or :spec-type ::type
;;                           :spec-var  ::type-var
;;                           :spec-any  any?))))

(defparser Function
  (s/and seq?
         #(some #{'->} %)
         (s/coll-of (s/or ::type ::type :_ #{'->})))
  (fn [x] (Function (map construct (butlast x)) (construct (last x)))))

(defmacro conform [x]
  `(do
     (s/def ::type ~(->> (sort @parsers)
                         (map (fn [[k v]] [k (list 'quote v)]))
                         (apply concat)
                         (cons 's/or)))
     (s/conform ::type ~x)))

(construct (conform '(a -> (b * c))))
