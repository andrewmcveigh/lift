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

;;; TODO: type class analogue
;;; TODO: type constraints
;;; TODO: multi let sugar
;;; TODO: macroexpand
;;; TODO: boot watch plugin

;;; TODO: Eq, Ord, Num, etc. typeclasses
;;; we already have protocols, which are not quite as good
;;; So, a better than protocol polymorphic dispatch, I.E.,
;;; * fmap f *F*
;;; * type constraints
;;; class Num a where

;;; Num is odd because clojure uses java's subtyping for this
;;; But it could be beneficial for optimizing numeric code without boxing

;;; Ord ... < <= > >= for non-numerics?

;;; TODO: pattern matching `case` & `defn` macros
;;; TODO: exhaustiveness checking


;; (instance Eq Nat)

;;; conflation
;;; type constraints
;;; parametric polymorphism
;;; ad-hoc polymorphism
;;; subtype polymorphism

;;; A thing is Eq if it has the functionality = or /=


;;; TODO: consider automatic varargs of binops

;; (syn/parse '(VCons 1 VNil))

;; (type/check (fn [y] (VCons y VNil)))

;; (data Expr
;;   = Var String
;;   | App Expr Expr
;;   | Lam Var Expr
;;   | Let Var Expr Expr
;;   | Lit Lit
;;   | If Expr Expr Expr)
;;; TODO: ^^ this "correctly" b0rks on 'Var not being a type - should it?
;;; is Var a type here? as part of a sum?
;;; Should it also lookup in value constructors?


;; (tdef defn Symbol -> (Vector Symbol) -> Expr -> Expr)
;; (type/check (defn dec [i] (- i 1)))
;; (macroexpand '(defn dec [i] (- i 1)))

;; (meta #'defn)

;;; what to do with macros?

;; (alter-var-root
;;  #'eval (fn [_]
;;           (fn [form]
;;             ;; (check/infer
;;             ;;  (check/map->Env (deref expr-env))
;;             ;;  (syn/parse form))
;;             (. clojure.lang.Compiler (eval form)))))

;; (dec 1)
