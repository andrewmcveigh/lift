(ns lift.shorthand
  ;; (:refer-clojure :exclude [defn])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as t]
   [clojure.string :as string]
   [lift.f.functor :as f]
   [orchestra.spec.test :as o])
  (:import
   [clojure.lang ISeq]))

(alias 'c 'clojure.core)

(declare type-seq parse-type-sig)

(def type-env (atom {}))

(defn res [s]
  (let [v (resolve s)
        ns (some-> v .ns .name str)
        name (some-> v .sym str)]
    (when (and ns name)
      (symbol ns name))))

(def word #"-|_|(?=[A-Z]+)")

(defn kebab-case [s]
  (->> (string/split s word)
       (c/map string/lower-case)
       (string/join "-")))

(defn predicate-name [x]
  (letfn [(predicate-ize [s]
            (->> (kebab-case s)
                 (format "%s-type?")))]
    (cond (symbol? x)  (c/symbol (predicate-ize (c/name x)))
          (keyword? x) (c/keyword (predicate-ize (c/name x)))
          (string? x)  (predicate-ize x)
          :else        (throw (ex-info (str "Don't know how to kebab:" x)
                                       {:type :unknown-type :x x})))))

(defn idx->key [i]
  (keyword (str (char (+ i 97)))))

(defn type-var
  ;; not really correct - this assumes too much:
  ;; that the type var is in the first position of :vars
  ;; and that only d-specs have type vars
  [dspec]
  (when (d-spec-type? dspec)
    (when-let [v (first (.-vars dspec))]
      (when (var-type? v) v))))

(defn pair-ks->-type-vars [args return]
  (map (fn [k v] [{k (:op v)} {(type-var v) [k]}])
       (conj (map (fn [x] [:args (idx->key x)])
                  (range 0 (count args))) [:ret])
       (conj args return)))

(defn dependent-fn [[[k v]]]
  [k (get-in @type-env [`dependent (res v) :f])])

(defn parse-fn [args return]
  (let [tvars (keep type-var (conj args return))]
    (when (> (count tvars) 1)
      (let [fs-vs (pair-ks->-type-vars args return)
            fs    (->> fs-vs (map dependent-fn) (into {}))
            vs    (->> (map second fs-vs)
                       (apply merge-with concat {})
                       (#(select-keys % tvars))
                       (remove (fn [[_ v]] (< (count v) 2)))
                       (into {}))]
        `(fn [~'x]
           ~(->> vs
                 (map (fn [[k v]]
                        `(= ~@(map #(list (res (get fs %))
                                          (list `get-in 'x %))
                                   v))))
                 (cons 'and)))))))

(defprotocol Base (base [_]))

(defprotocol Show (show [_]))

(defprotocol ToSpec (to-spec [_]))

(defmulti from-ast first)

(defn basetype? [x] (instance? Base x))

(defrecord BaseType [name fields])


(defmacro basetype
  {:style/indent [2 1]}
  [name fields & extends]
  ;; Should cache the hashcode in a mutable field
  `(do
     (deftype ~name ~(conj fields)
       Base
       (base [_] name)

       clojure.lang.IHashEq
       (equals [_# ~'other]
         (and (instance? ~name ~'other)
              ~@(map (fn [n] `(= (. ~'other ~(symbol (str \- n))) ~n))
                     fields)))
       (hasheq [_#] (.hasheq (BaseType. ~name ~fields)))
       (hashCode [_#] (.hashCode (BaseType. ~name ~fields)))

       ~@(when (seq fields)
           `[clojure.lang.ILookup
             (valAt
              [_# k#]
              (case k#
                ~@(apply concat (map (fn [j] [(keyword j) j]) fields))
                nil))
             (valAt
              [_# k# default#]
              (case k#
                ~@(apply concat (map (fn [j] [(keyword j) j]) fields))
                default#))])

       ~@(let [ex (apply array-map extends)]
           (apply concat
                  (if (contains? ex 'Show)
                    ex
                    `{Show (show [_] (apply pr-str ~fields))}))))

     (defn ~(predicate-name name) [~'x]
       (instance? ~name ~'x))

     (defmethod print-method ~name [~'x ~'writer]
       (.write ~'writer (show ~'x)))))


(basetype Unit []
  Show
  (show [_] "()"))

(defmethod from-ast ::Unit [_] (Unit.))

;; (basetype TType []
;;   Show
;;   (show [_] "type?")
;;   ToSpec
;;   (to-spec [_]
;;     `(s/or :fn (s/spec fn? :gen #(s/gen #{int?}))
;;            :spec (s/spec s/spec? :gen #(s/gen #{(s/spec int?)})))))

;; (basetype Value [value]
;;   ToSpec
;;   (to-spec [_] value))

(basetype Var [var]
  ToSpec
  (to-spec [_] (list 'quote var)))

(s/def ::Var
  (s/and simple-symbol? #(re-matches #"^[a-z]+$" (name %))))

(defmethod from-ast ::Var [[_ ast]] ast)


(basetype Predicate [pred]
  ToSpec
  (to-spec [_] pred))

(s/def ::Predicate
  (s/and symbol? #(re-matches #".+\?$" (name %))))

(defmethod from-ast ::Predicate [[_ ast]] ast)


(basetype Type [type args]
  Show
  (show [_]
    (format "%s %s" (pr-str type) (string/join " " (map pr-str args)))))

(s/def ::Type
  (s/cat :type ::predicate :args (s/+ ::type)))

(defmethod from-ast ::Type [[_ ast]]
  (Type. (:type ast) (map from-ast (:args ast))))


(basetype Tuple [fields]
  Show
  (show [_] (format "(%s)" (string/join " * " (map pr-str fields))))
  ToSpec
  (to-spec [_]
    `(s/tuple  ~@(map to-spec fields))))

(s/def ::Tuple
  (s/and seq? (s/cat :a* (s/* (s/cat :a ::type :* #{'*})) :a ::type)))

(defmethod from-ast ::Tuple [[_ ast]]
  (Tuple. (map from-ast (conj (mapv :a (:a* ast)) (:a ast)))))


(basetype List [a]
  Show
  (show [_] (format "(%s)" a))
  ToSpec
  (to-spec [_] `(s/coll-of ~(to-spec a))))

(s/def ::List
  (s/and seq? (s/cat :a ::type)))

(defmethod from-ast ::List [[_ ast]]
  (List. (from-ast (:a ast))))


(basetype Vector [a]
  FromAst
  (from-ast [ast]
    (Vector. (from-ast (:a ast))))
  Show
  (show [_] (format "[%s]" a))
  ToSpec
  (to-spec [_] `(s/coll-of ~(to-spec a) :kind vector?)))

(s/def ::Vector
  (s/and vector? (s/cat :a ::type)))

(defmethod from-ast ::Vector [[_ ast]]
  (Vector. (from-ast (:a ast))))


(basetype Expr [op args])

(s/def ::Expr
  (s/and seq?
         (s/cat :op symbol? :args (s/+ (s/or ::Var ::Var ::Value ::Value)))))

(defmethod from-ast ::Expr [[_ ast]]
  (Expr. (:op ast) (map from-ast (:args ast))))


(basetype Function [args return]
  ISeq
  (seq [_] (concat args [return]))

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
              ~@(when-let [f (parse-fn args return)] [:fn f])
              :ret ~(to-spec return))))

(basetype Spec [op args]
  Show
  (show [_]
    (format "%s %s" op (string/join " " (map pr-str args))))
  ToSpec
  (to-spec [_] `(~op ~@(map to-spec args))))

(basetype DSpec [op vars args]
  ISeq
  (seq [_] (apply list op args))
  Show
  (show [_]
    (let [sig (get-in @type-env [`dependent (res op) :sig])
          dep-type (first (type-seq sig))]
      (format "(%s: %s ** %s %s)"
              (string/join ", " (map pr-str vars))
              (pr-str dep-type)
              (pr-str op)
              (string/join " " (map pr-str args)))))
  ToSpec
  (to-spec [x] `(~op ~@(map to-spec args))))

(defn not->? [x] (not= x '->))

(defn ->? [decl]
  (some (partial = '->) decl))

(s/def ::-> (partial = '->))

(s/def ::func-sig
  (s/and seq? #(some #{'->} %)))

(s/def ::spec
  (s/cat :op symbol?
         :args (s/+ (s/or :spec-type ::type
                          :spec-var  ::type-var
                          :spec-any  any?))))

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

(defn dependent? [x]
  (contains? (get @type-env `dependent) (res x)))

(defn parse-spec [[t x]]
  (case t
    :type (TType.)
    :pred (Predicate. x)
    :bfun (parse-type-sig x)
    :coll (parse-spec x)
    :list (List. (parse-spec (:a x)))
    :vect (Vector. (parse-spec (:a x)))
    :tupl (Tuple. (concat [(parse-spec (:a x))
                           (parse-spec (:b x))]
                          (map parse-spec (:cs x))))
    :tsig (Type. (:op x) (map parse-spec (:args x)))
    ;; is a typesig, not a spec
    ;; but what is it?
    ;; do I know yet? it's a function that returns a type?
    :expr (Expr. (apply list (:op x) (map parse-spec (:args x))))
    :expr-var (Var. x)
    :expr-val (Value. x)
    :spec (let [{:keys [op args]} x]
            (if (dependent? op)
              (let [args (map parse-spec args)]
                (DSpec. op (filter var-type? args) args))
              (Spec.  op (map parse-spec args))))
    :spec-type (parse-spec x)
    :spec-var  (Var. x)
    :spec-any  (Value. x)))

(defn type-seq [coll]
  (if coll
    (lazy-seq
     (cons (let [a (take-while not->? coll)]
             (if (contains? (set a) '->)
               (type-seq a)
               (parse-spec (s/conform ::type a))))
           (type-seq (next (drop-while not->? coll)))))
    ()))

(defn parse-type-sig [sig]
  (let [tseq (type-seq sig)]
    (if (> (count tseq) 1)
      (Function. (butlast tseq) (last tseq))
      (first tseq))))

(defmacro sdef [f & sig]
  `(s/def ~f ~(to-spec (parse-type-sig sig))))

(defn check-ns [ns]
  (let [syms (set (filter #(and (symbol? %) (= (name ns) (namespace %)))
                          (o/instrumentable-syms)))]
    (o/unstrument)
    (o/instrument syms)
    (t/check syms {:clojure.spec.test.check/opts {:num-tests 10}})))

(defmacro check [sym & [n]]
  `(do
     (o/unstrument)
     (o/instrument '~(res sym))
     (let [res# (-> '~(res sym)
                    (t/check {:clojure.spec.test.check/opts
                              {:num-tests (or ~n 100)}})
                    (first)
                    (:clojure.spec.test.check/ret)
                    (:result))]
       (or (true? res#)
           (prn res#)
           (:clojure.spec.alpha/problems (ex-data res#))))))

;; (defmacro dependent
;;   {:style/indent :defn}
;;   [t sig args f expr]
;;   (let [sig sig]
;;     (swap! type-env
;;            assoc-in
;;            [`dependent (res t)] {:sig sig :args args :f f}))
;;   `(defn ~t ~args ~expr))

;;; We need a spec
;;; That also returns a value to the env when called with a value

;;; Something that is, a) dependent, and b) -> type? is a spec/type
;;; This must put some information into the type environment

;; (dependent vect? (nat-int? -> type? -> type?) [n t] count
;;   (s/coll-of t :into [] :kind vector?))

;; if there's a spec in the registry by the name
;; pull the type sig:
;; a. does it return a type?
;; b. does it have a non-type? argument
;; => then it's a dependent type, right?
;; c. now, where does the dependent-fn go?
;; (defmacro defn
;;   {:style/indent 2}
;;   [name & args]
;;   `(let [v# (c/defn ~name ~@args)]
;;      (if ~(contains? (s/registry) (res name))
;;        (let [res# (check ~name 1)]
;;          (if (true? res#)
;;            v#
;;            (do
;;              (ns-unmap *ns* ~name)
;;              res#)))
;;        v#)))

;; (sdef length nat-int? -> type?)
;; (defn length [n]
;;   (fn [xs] (= (count xs) n)))

;; If a type is dependent on a value, the value is part of the type.
;; if we need a function/spec to check on a type, we need the function
;; to check the value.

;; or there's a better way to check vect of length n
;; type-predicate?
;; value-predicate?

;; (sdef vect?, nat-int? -> type? -> type?)
(defn vect? [n]
  (fn [t]
    (s/and (s/coll-of t :into [] :kind vector?)
           (length n))))

'count -> n

;; '(data Vect (len :- Nat -> elem :- Type -> Type)
;;    (Nil  (Vect Z elem))
;;    (cons (x :- elem -> xs :- Vect len elem -> Vect (S len) elem)))

;;: data Vect : (len : Nat) -> (elem : Type) -> Type where
;;:   Nil  : Vect Z elem
;;:   (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem

;; what is a type in this?

;; vect? : nat-int? -> type? -> type?
;; vect? Z t = vect
;; ()

;; (s/conform (vect? 2 int?) [3]) -> should fail

;;: concat : vect? n int? -> vect? m int? -> vect? (+ n m) int?
;;: (Dependent. env)
;;: env = {'n count 'm str}

;;; This says that all instances of type-variable n, in this type
;;; scope, must be the same.
;;; And, that you can derive n from a value with the function count
;;; Types that can be dependent are product types and functions
;;; Container types can have type vars that vary dependent types

;;: * Should a type that is dependent be a different basetype?
;;: * How do we express a type scope & env in something?

;;; We want to parse:
;;: (sdef  f, vect? n string? -> vect? n nat-int?)
;;; Into:
;;: (s/def f  (s/fspec
;;:            :args (s/cat :a (s/coll-of string? :into [] :kind vector?))
;;:            :fn  #(= (-> % :args :a count) (-> % :ret count))
;;:            :ret  (s/coll-of int? :into [] :kind vector?)))
