(ns lift.shorthand
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as t]
   [clojure.string :as string]
   [orchestra.spec.test :as o]))

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
(basetype Predicate [pred])
(basetype Record    [kvs])
(basetype List      [type])
(basetype Vector    [type])
(basetype Set       [type])
(basetype Pair      [a b])
(basetype Tuple     [fields])
(basetype Function  [args return])
(basetype Spec      [op args])

(defmethod print-method Unit [x writer]
  (.write writer "()"))

(defmethod print-method Value [x writer]
  (.write writer (str (.-value x))))

(defmethod print-method Predicate [x writer]
  (.write writer (str (.-pred x))))

(defmethod print-method Record [x writer]
  (.write writer (pr-str (apply array-map (.-kvs x)))))

(defmethod print-method Pair [x writer]
  (.write writer (format "[%s %s]" (pr-str (.-a x)) (pr-str (.-b x)))))

(defmethod print-method Tuple [x writer]
  (.write writer (format "[%s]" (string/join " " (map pr-str (.-fields x))))))

(defmethod print-method List [x writer]
  (.write writer (format "(%s)" (pr-str (.-type x)))))

(defmethod print-method Vector [x writer]
  (.write writer (format "[%s]" (pr-str (.-type x)))))

(defmethod print-method Set [x writer]
  (.write writer (format "#{%s}" (pr-str (.-type x)))))

(defmethod print-method Function [x writer]
  (.write writer (format "(%s -> %s)"
                         (string/join " -> " (map pr-str (.-args x)))
                         (pr-str (.-return x)))))

(defmethod print-method Spec [x writer]
  (.write writer (format "%s %s"
                         (.-op x)
                         (string/join " " (map pr-str (.-args x))))))

(defn idx->key [i]
  (keyword (str (char (+ i 97)))))

(extend-protocol Type
  Value
  (to-spec [x] (.-value x))
  Predicate
  (to-spec [x] (.-pred x))
  Pair
  (to-spec [x] `(s/tuple ~(to-spec (.-a x)) ~(to-spec (.-b x))))
  Tuple
  (to-spec [x] `(s/tuple ~(to-spec (.-a x))
                         ~(to-spec (.-b x))
                         ~@(map to-spec (.-cs x))))
  List
  (to-spec [x] `(s/coll-of ~(to-spec ~(.-a x))))
  Vector
  (to-spec [x] `(s/coll-of ~(to-spec ~(.-a x)) :kind vector?))
  Set
  (to-spec [x] `(s/coll-of ~(to-spec ~(.-a x)) :kind set?))
  Function
  (to-spec [x] `(s/fspec
                 :args ~(->> (.-args x)
                             (map-indexed (fn [i y] [(idx->key i) (to-spec y)]))
                             (apply concat)
                             (cons `s/cat))
                 :ret ~(to-spec (.-return x))))
  Spec
  (to-spec [x] `(~(.-op x) ~@(map to-spec (.-args x)))))

(defn not->? [x] (not= x '->))

(defn ->? [decl]
  (some (partial = '->) decl))

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
                      :args (s/+ (s/alt :spec-type ::type :spec-any any?)))))

;; (s/explain ::type (.-v (first (value-seq '(vect? 4 nat-int?)))))

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
    :spec (Spec. (:op x) (map parse-spec (:args x)))
    :spec-type (parse-spec x)
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
     (o/instrument '~(#'s/res sym))
     (let [res# (-> '~(#'s/res sym)
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

(defmacro vect? [n t]
  `(s/coll-of ~t :into [] :kind vector? :min-count ~n :max-count ~n))
