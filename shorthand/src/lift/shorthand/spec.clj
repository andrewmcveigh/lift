(ns lift.shorthand.spec
  (:require
   [clojure.spec.alpha :as s]
   [lift.shorthand.util :refer [res]]))

(def parsers (atom {}))

(defmacro defparser [t spec]
  (if-let [sym (res t)]
    `(do
       (swap! parsers assoc (keyword (name ~sym)) ~sym)
       (s/def ~sym ~spec))
    (throw (Exception. (str "Could not resolve `t`: " (pr-str t))))))

(defparser Var
  (s/and simple-symbol? #(re-matches #"^[a-z]+$" (name %))))

(defparser Predicate
  (s/and symbol? #(re-matches #".+\?$" (name %))))

(defparser Type
  (s/cat :type ::predicate :args (s/+ ::type)))

(defparser Tuple
  (s/and seq? (s/cat :a* (s/* (s/cat :a ::type :* #{'*})) :a ::type)))

(defparser List
  (s/and seq? (s/cat :a ::type)))

(defparser Vector
  (s/and vector? (s/cat :a ::type)))

(defparser Expr
  (s/and seq? (s/cat :op symbol? :args (s/+ (s/or ::Var `Var ::Value `Value)))))

(defparser Spec
  (s/cat :op symbol?
         :args (s/+ (s/or :spec-type ::type
                          :spec-var  ::type-var
                          :spec-any  any?))))

(defparser Function
  (s/and seq?
         #(some #{'->} %)
         (s/coll-of (s/or ::type (partial not= '->) :_ #{'->}))))

(defmacro conform [x]
  `(s/conform ~(cons 's/or (apply concat (sort @parsers))) x))

(s/def ::type
  (s/or :type #{'type?}
        :pred ::predicate
        :tvar ::type-var
        :bfun ::func-sig
        :list ::list-a
        :vect ::vector-a
        :tupl ::tuple-a
        :tsig ::type-sig
        :expr ::type-expr
        :spec ::spec))

(s/def ::re-type
  (s/alt :type #{'type?}
         :pred ::predicate
         :tvar ::type-var
         :bfun ::func-sig
         :list ::list-a
         :vect ::vector-a
         :tupl ::tuple-a
         :tsig ::type-sig
         :expr ::type-expr
         :spec ::spec))

(defmulti from-ast first)

(defmethod from-ast ::Unit [_] (Unit))

(defmethod from-ast ::Var [[_ ast]] ast)

(defmethod from-ast ::Predicate [[_ ast]] ast)

(defmethod from-ast ::Tuple [[_ ast]]
  (Tuple (map from-ast (conj (mapv :a (:a* ast)) (:a ast)))))

(defmethod from-ast ::Type [[_ ast]]
  (Type (:type ast) (map from-ast (:args ast))))

(defmethod from-ast ::List [[_ ast]]
  (List (from-ast (:a ast))))

(defmethod from-ast ::Vector [[_ ast]]
  (Vector (from-ast (:a ast))))

(defmethod from-ast ::Expr [[_ ast]]
  (Expr (:op ast) (map from-ast (:args ast))))
