(ns lift.shorthand
  ;; (:refer-clojure :exclude [defn])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as t]
   [clojure.string :as string]
   [lift.shorthand.impl :refer :all]
   [lift.shorthand.util :refer :all]
   [orchestra.spec.test :as o])
  (:import
   [clojure.lang ISeq]))

(alias 'c 'clojure.core)

(declare type-seq parse-type-sig)

(def type-env (atom {}))

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
  ISeq
  (seq [_] (concat args [return]))

  Show
  (show [_]
    (format "(%s -> %s)"
            (string/join " -> " (map pr-str args))
            (pr-str return)))

  ;; ToSpec
  #_(to-spec [x]
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
