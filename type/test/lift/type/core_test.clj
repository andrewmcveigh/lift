(ns lift.type.core-test
  (:refer-clojure :exclude [class dec defn inc not not= type])
  (:require
   [clojure.test :refer [deftest is]]
   [lift.type.core :as type :refer [data defn tdef T type]]
   [lift.type.check :as check]
   [lift.type.syntax :as syn]
   [lift.type.type :as t]
   [lift.type.util :as u]
   [lift.type.parse :as parse]
   [clojure.spec.alpha :as s]
   [lift.type.spec :as spec])
  (:import
   [clojure.lang ExceptionInfo]))

(alias 'c 'clojure.core)

(deftest tdef-test
  (is
   (try
     (tdef should-break Wonk -> Stink)
     false
     (catch ExceptionInfo e
       (-> e ex-data :type (= ::parse/unknown-type))))))

(defmacro cannot-unify [expr & [a b]]
  `(try
     ~expr
     false
     (catch ExceptionInfo e#
       (let [data# (ex-data e#)]
         (is (-> data# :type (= ::check/ex-cannot-unify)))
         (when ~a (is (-> data# :a (= ~a))))
         (when ~b (is (-> data# :b (= ~b))))))))

(deftest expr-type-check-test

  (is (= (T Bool) (type/check True)))
  (is (= (T Bool) (type/check False)))

  (is (= (T Bool)
         (type/check (let [x 1] (even? (+ x 2))))))

  (is (= (T Int)
         (type/check (let [x 1] (dec (inc (+ x 2)))))))

  (is (= (T Bool)
         (type/check (let [x 1]
                       (let [y (dec (inc (+ x 2)))]
                         (<= x (dec y)))))))

  (cannot-unify
   (type/check (let [x 1]
                 (let [y (dec (inc (+ x 2)))]
                   (+ 2 (<= x (dec y))))))
   (T Int)
   (T Bool))

  (cannot-unify
   (type/check (== 1 ""))
   (T Int)
   (T String)))


(deftest list-test
  (is (= (t/Sum 'List [(t/Const 'Int)])
         (type/check (Cons 1 Nil)))))

(deftest vector-test
  (is (= (t/Sum 'Vector [(t/Const 'Int)])
         (type/check (VCons 1 VNil)))))

(deftest map-test
  (is (= (t/Sum 'List [(t/Const 'Int)])
         (type/check (map inc (Cons 1 Nil))))))

apply

;;; TODO: multi let sugar
;;; TODO: macroexpand
;;; TODO: boot watch plugin
;;; TODO: fancy import of prelude
;;; TODO: type checking stack errors
;;; TODO: auto gen `deriving` classes
;;; TODO: casting between Num a types
;;; TODO: pattern matching `case` & `defn` macros
;;; TODO: exhaustiveness checking
;;; TODO: Nat type - what is a Nat?
;;; TODO: consider automatic varargs of binops
;;; TODO: what to do with macros?
;;; TODO: Constrained to Var unification rule

;;; TODO: `case`
;;; How to do this? It's a syntax extension. The easiest thing would be to treat
;;; it as if it's a syntax form.
;;; But... it's a macro. If we can add macros typing to the type system, that's
;;; going to be super cool.
;;; You'd also need to add pattern matching to the type system, how?

;;; TODO: WTF is a boundary!?
;;; How do we keep clojure from calling into this code, with garbage values?
;;; Have functions not be callable? - then functions would have to be something
;;; else... interesting...
;;; Clojure can call anything in java. But we don't have to make it easy.
;;; Separate namespace things, clojure doesn't have _easy_ access to
;;; Non-callable from clojure things
;;; You'd need some intermediate interpreter - slow!

;; #lift/lang typed
