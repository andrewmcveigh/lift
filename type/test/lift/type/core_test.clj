(ns lift.type.core-test
  (:refer-clojure :exclude [defn inc dec])
  (:require
   [clojure.test :refer [deftest is]]
   [lift.type.core :as type :refer [data defn tdef T]]
   [lift.type.check :as check]
   [lift.type.syntax :as syn]
   [lift.type.type :as t]
   [lift.type.util :as u]
   [lift.type.parse :as parse])
  (:import
   [clojure.lang ExceptionInfo]))

(alias 'c 'clojure.core)

(type/const Char char?)
(type/const Int integer?)
(type/const Double double?)
(type/const String string?)

(data Bool = True | False)

(deftest tdef-test
  (is
   (try
     (tdef should-break Wonk -> Stink)
     false
     (catch ExceptionInfo e
       (-> e ex-data :type (= ::parse/unknown-type))))))

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

(data List a = Nil | Cons a (List a))

(data Vector a = VNil | VCons a (Vector a))

;; (get @t/expr-env (u/ns-qualify 'VCons))

(tdef = a -> a -> Bool)

(= (t/Sum 'Vector ['Int])
   (type/check (VCons 1 VNil)))

;; (syn/parse '(VCons 1 VNil))

;; (type/check (fn [y] (VCons y VNil)))

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
