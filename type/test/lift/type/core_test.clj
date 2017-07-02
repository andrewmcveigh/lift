(ns lift.type.core-test
  (:refer-clojure :exclude [defn inc dec])
  (:require
   [clojure.test :refer [deftest is]]
   [lift.type.core :as type :refer [data defn tdef]]
   [lift.type.check :as check]
   [lift.type.syntax :as syn]
   [lift.type.type :as t]
   [lift.type.util :as u])
  (:import
   [clojure.lang ExceptionInfo]))

(alias 'c 'clojure.core)

(data Bool = True | False)

(tdef + Int -> Int -> Int)
(tdef * Int -> Int -> Int)
(tdef / Int -> Int -> Int)
(tdef - Int -> Int -> Int)

(tdef <  Int -> Int -> Bool)
(tdef <= Int -> Int -> Bool)
(tdef >  Int -> Int -> Bool)
(tdef >= Int -> Int -> Bool)
(tdef == Int -> Int -> Bool)

(tdef even? Int -> Bool)
(tdef odd?  Int -> Bool)

(tdef inc Int -> Int)
(defn inc [i] (+ i 1))

(tdef dec Int -> Int)
(defn dec [i] (- i 1))

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

  (is (= (t/Sum 'Bool []) (type/check True)))

  (is (= (t/Sum 'Bool [])
         (type/check (let [x 1] (even? (+ x 2))))))

  (is (= (t/Const 'Int)
         (type/check (let [x 1] (dec (inc (+ x 2)))))))

  (is (= (t/Const 'Bool)
         (type/check (let [x 1]
                       (let [y (dec (inc (+ x 2)))]
                         (<= x (dec y)))))))

  (cannot-unify
   (type/check (let [x 1]
                    (let [y (dec (inc (+ x 2)))]
                      (+ 2 (<= x (dec y))))))
   (t/Const 'Int)
   (t/Const 'Bool))

  (cannot-unify
   (type/check (== 1 ""))
   (t/Const 'Int)
   (t/Const 'String)))

(data List a = Nil | Cons a (List a))

(data Vector a = VNil | VCons a (Vector a))

;; (get @t/expr-env (u/ns-qualify 'VCons))

(tdef = a -> a -> Bool)

(= (t/Sum 'Vector ['Int])
   (type/check (VCons 1 VNil)))

;; (syn/parse '(VCons 1 VNil))

;; (type/check (fn [y] (VCons y VNil)))


(type/check True)



;;; TODO: literal True doesn't check due to no resolution

;; (type/check (VCons 1 VNil))
;;; TODO: above ^^ doesn't check due to no resolution

;; (VCons 1 VNil)
;;; TODO: ^^ how to get type?

;; (keys @t/expr-env)

(data Expr
  = Var String
  | App Expr Expr
  | Lam Var Expr
  | Let Var Expr Expr
  | Lit Lit
  | If Expr Expr Expr)

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
