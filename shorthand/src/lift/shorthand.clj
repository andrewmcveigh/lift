(ns lift.shorthand
  ;; (:refer-clojure :exclude [defn])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as t]
   [clojure.string :as string]
   [lift.shorthand.impl :as impl :refer :all]
   [lift.shorthand.util :refer :all]
   [orchestra.spec.test :as o])
  (:import
   [clojure.lang ISeq]))

(alias 'c 'clojure.core)

(def type-env (atom {}))

(defmacro sdef [sym & sig]
  `(do
     (swap! type-env assoc '~(ns-qualify sym) (parse-type-signature '~sig))
     (s/def ~sym ~(to-spec (parse-type-signature sig)))))

(defmacro vdef [sym sig]
  `(def ~sym (parse ::impl/type '~sig)))

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

(sdef env?, type?)
(vdef env? (int? * int?))

(s/def ::bindings
  (s/and vector? (s/+ (s/cat :sym simple-symbol? :form any?))))

(s/def ::data
  (s/cat :where    #{'where}
         :bindings ::bindings
         :expr     (s/or :spec seq? :pred `Predicate)))

{:where where,
 :bindings [{:sym n, :form count}
            {:sym t, :form (comp (partial reduce unify t) (partial map type))}],
 :expr [:spec (s/coll-of t :into [] :kind vector? :min-count n :max-count n)]}

(defmacro where
  {:style/indent :defn}
  [bindings spec]
  (assert nil "`where` not used inside `data`"))

(defmacro data
  {:style/indent :defn}
  [sym args expr]
  (let [ns-sym (ns-qualify sym)
        conformed (s/conform ::data expr)]
    (if (= ::s/invalid conformed)
      (throw
       (ex-info "`expr` did not conform to spec"
                {:type ::s/invalid :explain-data (s/explain-data ::data expr)}))
      (let [{:keys [bindings expr]} conformed
            bound-syms (mapv :sym bindings)
            bindings (->> bindings
                          (map (fn [{:keys [sym form]}]
                                 [(list 'quote sym) form]))
                          (into (array-map)))]
        `(defn ~sym ~args
           (Dependent
            '~sym
            ~args
            ~bindings
            (get @type-env '~ns-sym)
            (fn ~args ~(second expr))))))))

(sdef vect?, nat-int? -> type? -> type?)
(data vect? [n t]
  (where [n count
          t (comp (partial reduce 'unify t) (partial map type))]
    (s/coll-of t :into [] :kind vector? :min-count n :max-count n)))

(vect? '1 'int?)

;;

;;; We need a spec
;;; That also returns a value to the env when called with a value

;;; Something that is, a) dependent, and b) -> type? is a spec/type
;;; This must put some information into the type environment

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
;; (defn vect? [n]
;;   (fn [t]
;;     (s/and (s/coll-of t :into [] :kind vector?)
;;            (length n))))

;; 'count -> n

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
