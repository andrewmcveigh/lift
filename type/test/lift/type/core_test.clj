(ns lift.type.core-test
  (:refer-clojure :exclude [defn])
  (:require
   [clojure.test :refer [deftest is]]
   [lift.type.core :as type :refer [data defn tdef]]
   [lift.type.check :as check]
   [lift.type.syntax :as syn]
   [lift.type.type :refer [type-env expr-env]]))

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

;; (check/infer
;;  (check/map->Env @expr-env)
;;  (syn/parse '(let [x 1] (even? (+ x 2)))))

(tdef inc Int -> Int)
(defn inc [i] (+ i 1))

(tdef dec Int -> Int)
(defn dec [i] (- i 1))

(data List a = Nil | Cons a (List a))

(data Vector a = VNil | VCons a (Vector a))

;;; TODO: better return for `data`
;;; TODO: ftv in product/sum -> scheme?

(VCons 1 VNil)

expr-env

(data Expr
  = Var String
  | App Expr Expr
  | Lam Var Expr
  | Let Var Expr Expr
  | Lit Lit
  | If Expr Expr Expr)

(tdef defn Symbol -> (Vector Symbol) -> Expr -> Expr)
(type/check (defn dec [i] (- i 1)))
(macroexpand '(defn dec [i] (- i 1)))

(meta #'defn)

;;; what to do with macros?

;; (alter-var-root
;;  #'eval (fn [_]
;;           (fn [form]
;;             ;; (check/infer
;;             ;;  (check/map->Env (deref expr-env))
;;             ;;  (syn/parse form))
;;             (. clojure.lang.Compiler (eval form)))))

;; (dec 1)
