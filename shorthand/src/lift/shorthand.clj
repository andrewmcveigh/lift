(ns lift.shorthand
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as t]
   [clojure.string :as string]
   [orchestra.spec.test :as o]))

(def type-env (atom {}))

(defn res [s]
  (let [v (resolve s)
        ns (some-> v .ns .name str)
        name (some-> v .sym str)]
    (when (and ns name)
      (symbol ns name))))

(alias 'c 'clojure.core)

(defprotocol Base (base [_]))

(defn basetype? [x]
  (instance? Base x))

(defrecord BaseType [name fields])

(defmacro basetype
  {:style/indent 2}
  [name fields & extends]
  ;; Should cache the hashcode in a mutable field
  `(deftype ~name ~fields
     Base
     (base [_] name)
     clojure.lang.IHashEq
     (equals [_# ~'other]
       (and (instance? ~name ~'other)
            ~@(map (fn [n] `(= (. ~'other ~(symbol (str \- n))) ~n))
                   fields)))
     (hasheq [_#] (.hasheq (BaseType. ~name ~fields)))
     (hashCode [_#] (.hashCode (BaseType. ~name ~fields)))
     ~@extends))

(defprotocol Type (to-spec [_]))

(basetype Unit      [])
(basetype Value     [value])
(basetype Var       [var])
(basetype Predicate [pred])
(basetype Record    [kvs])
(basetype List      [a])
(basetype Vector    [a])
(basetype Set       [a])
(basetype Pair      [a b])
(basetype Tuple     [fields])
(basetype Function  [args return])
(basetype Spec      [op args])
(basetype DSpec     [op vars args])

(defn type-var? [x] (instance? Var x))
(defn dspec?    [x] (instance? DSpec x))

(defmethod print-method Unit [x writer]
  (.write writer "()"))

(defmethod print-method Value [x writer]
  (.write writer (str (.-value x))))

(defmethod print-method Var [x writer]
  (.write writer (str (.-var x))))

(defmethod print-method Predicate [x writer]
  (.write writer (str (.-pred x))))

(defmethod print-method Record [x writer]
  (.write writer (pr-str (apply array-map (.-kvs x)))))

(defmethod print-method Pair [x writer]
  (.write writer (format "[%s %s]" (pr-str (.-a x)) (pr-str (.-b x)))))

(defmethod print-method Tuple [x writer]
  (.write writer (format "[%s]" (string/join " " (map pr-str (.-fields x))))))

(defmethod print-method List [x writer]
  (.write writer (format "(%s)" (pr-str (.-a x)))))

(defmethod print-method Vector [x writer]
  (.write writer (format "[%s]" (pr-str (.-a x)))))

(defmethod print-method Set [x writer]
  (.write writer (format "#{%s}" (pr-str (.-a x)))))

(defmethod print-method Function [x writer]
  (.write writer (format "(%s -> %s)"
                         (string/join " -> " (map pr-str (.-args x)))
                         (pr-str (.-return x)))))

(defmethod print-method Spec [x writer]
  (.write writer (format "%s %s"
                         (.-op x)
                         (string/join " " (map pr-str (.-args x))))))

(defmethod print-method DSpec [x writer]
  (let [op (.-op x)
        sig (get-in @type-env [`dependent (res op) :sig])
        dep-type (first (type-seq sig))]
    (.write writer (format "(%s: %s ** %s %s)"
                                 (string/join ", " (map pr-str (.-vars x)))
                                 (pr-str dep-type)
                                 op
                                 (string/join " " (map pr-str (.-args x)))))))

(defn idx->key [i]
  (keyword (str (char (+ i 97)))))

(defn type-var [dspec]
  (when (dspec? dspec)
    (when-let [v (first (.-vars dspec))]
      (when (type-var? v) v))))

(defn op [dspec]
  (when (dspec? dspec) (.-op dspec)))

(defn parse-fn
  "given a list of args and return, find which elements are:
   a) dependent, and if there are two or more
   b) if they are dependent on each other, then
   c) pull the respective f from env by op
   d) write into :fn spec

   we know that (fa :a) and (fr :ret) must be equal because they use n
   :fn (let [fa (:f env-a)
             fr (:f env-r)]
         (fn [x] (= (-> x :args :a fa)
                    (-> x :ret fr))))"
  [f]
  (let [args   (.-args f)
        return (.-return f)
        tvars  (keep type-var (conj args return))]
    (when (> (count tvars) 1)
      (let [fs-vs (->> (conj args return)
                       (map (fn [k v] [[k (op v)] {(type-var v) [k]}])
                            (conj (map (juxt (constantly :args) idx->key)
                                       (range 0 (count args))) [:ret])))
            fs (->> fs-vs
                    (map (fn [[[k v]]]
                           [k (get-in @type-env [`dependent (res v) :f])]))
                    (into {}))
            vs (->> (map second fs-vs)
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

(extend-protocol Type
  Value
  (to-spec [x] (.-value x))
  Var
  (to-spec [x] (list 'quote(.-var x)))
  Predicate
  (to-spec [x] (.-pred x))
  Pair
  (to-spec [x] `(s/tuple ~(to-spec (.-a x)) ~(to-spec (.-b x))))
  Tuple
  (to-spec [x] `(s/tuple ~(to-spec (.-a x))
                         ~(to-spec (.-b x))
                         ~@(map to-spec (.-cs x))))
  List
  (to-spec [x] `(s/coll-of ~(to-spec (.-a x))))
  Vector
  (to-spec [x] `(s/coll-of ~(to-spec (.-a x)) :kind vector?))
  Set
  (to-spec [x] `(s/coll-of ~(to-spec (.-a x)) :kind set?))
  Function
  (to-spec [x] (let [f (parse-fn x)]
                 `(s/fspec
                   :args ~(->> (.-args x)
                               (map-indexed
                                (fn [i v] [(idx->key i) (to-spec v)]))
                               (apply concat)
                               (cons `s/cat))
                   ~@(when f [:fn f])
                   :ret ~(to-spec (.-return x)))))
  Spec
  (to-spec [x] `(~(.-op x) ~@(map to-spec (.-args x))))
  DSpec
  (to-spec [x] `(~(.-op x) ~@(map to-spec (.-args x)))))

(to-spec (parse-type-sig '(vect? n int? -> vect? n int?)))

(defn not->? [x] (not= x '->))

(defn ->? [decl]
  (some (partial = '->) decl))

(s/def ::type-var
  (s/and simple-symbol? #(re-matches #"^[a-z]+$" (name %))))

(s/def ::predicate
  (s/and symbol? #(re-matches #".+\?$" (name %))))

(s/def ::-> (partial = '->))

(s/def ::list-a
  (s/and seq? (s/cat :a ::type)))

(s/def ::vector-a
  (s/and vector? (s/cat :a ::type)))

(s/def ::set-a
  (s/and set? (s/cat :a ::type)))

(s/def ::pair-a
  (s/and vector? (s/cat :a ::type :b ::type)))

(s/def ::n-tuple-a
  (s/and vector? (s/cat :a ::type :b ::type :cs (s/+ ::type))))

(s/def ::type
  (s/alt :pred ::predicate
         :bfun (s/and seq? #(some #{'->} %))
         :coll (s/or :list ::list-a
                     :vect ::vector-a
                     :set  ::set-a
                     :pair ::pair-a
                     :tupl ::n-tuple-a)
         :spec (s/cat :op symbol?
                      :args (s/+ (s/or :spec-type ::type
                                       :spec-var  ::type-var
                                       :spec-any  any?)))))

;; [n, int? ** vect? n int?]
;; [2 [3 4]]
;; (s/explain ::type (.-v (first (value-seq '(vect? 4 nat-int?)))))

;; data DPair : (a : Type) -> (P : a -> Type) -> Type where
;;     MkDPair : {P : a -> Type} -> (x : a) -> P x -> DPair a P
;; (n : Nat ** Vect n Int)
;; (2, [3 4])
;; DPair Nat (\n -> Vect n Int)
;; MkPair 2 [3 4]
;; (int? -> int?) /= ((int? -> int?))
;; (int? -> int? -> int?) /= (int? -> (int? -> int?))
;; int? -> {:x int?}

;;: TODO:
;;: * Varargs functions
;;:   (fdef +, int? -> (s/+ int?) -> int?)
;;: * Each type of type can be represented by deftype
;;: * Sum type shorthand expands to s/or (with/out destructuring)
;;;   - But how would that work? We can write s/or in a spec without it
;;;     being a "type" in the registry
;;: * What is the difference between a spec and a product?
;;;   - Is there one? - I don't think there is
;;;   - So what do we call it?
;;;   - cplx

(declare parse-type-sig)

(defn dependent? [x]
  (contains? (get @type-env `dependent) (res x)))

(defn parse-spec [[t x]]
  (case t
    :pred (Predicate. x)
    :bfun (parse-type-sig x)
    :coll (parse-spec x)
    :list (List. (parse-spec (:a x)))
    :vect (Vector. (parse-spec (:a x)))
    :set  (Set. (parse-spec (:a x)))
    :pair (Pair. (parse-spec (:a x))
                 (parse-spec (:b x)))
    :tupl (Tuple. (concat [(parse-spec (:a x))
                           (parse-spec (:b x))]
                          (map parse-spec (:cs x))))
    :spec (let [{:keys [op args]} x]
            (if (dependent? op)
              (let [args (map parse-spec args)]
                (DSpec. op (filter type-var? args) args))
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

(to-spec (parse-type-sig '(vect? n int? -> vect? n int?)))

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

(defmacro tdef
  {:style/indent 2}
  [name & args]
  `(do
     (defn ~name ~@args)
     (check ~name 1)))

(defmacro dependent
  {:style/indent :defn}
  [t sig args f expr]
  (swap! type-env assoc-in [`dependent (res t)] {:sig sig :args args :f f})
  `(defn ~t ~args ~expr))

;;; We need a spec
;;; That also returns a value to the env when called with a value
;;; (fn [a] (

;;; Something that is, a) dependent, and b) -> type? is a spec/type
;;; This must put some information into the type environment
(dependent vect? (nat-int? -> type? -> type?) [n t] count
  (s/coll-of t :into [] :kind vector?))

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
