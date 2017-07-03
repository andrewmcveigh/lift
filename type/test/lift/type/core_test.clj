(ns lift.type.core-test
  (:refer-clojure :exclude [= class dec defn inc not not= type])
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

(tdef map (a -> b) -> (List a) -> (List b))
;; (tdef quote a -> a)

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

(defmacro class
  {:style/indent [2 1]}
  [& decl]
  (let [{:keys [class sigs impls]} (s/conform ::spec/class decl)
        class (second class)
        t (:type-name class)
        constrained (map parse/construct (:args class))
        constraint  (t/Constraint t constrained)
        constraint' `(->> '~(:args class)
                          (map parse/construct)
                          (t/Constraint '~t))]
    `(do
       (swap! t/type-env assoc '~t '~constraint')
       ~@(mapcat
          (fn [{:keys [f sig]}]
            (let [f (u/resolve-sym f)
                  sig `(t/Constrained ~constraint'(parse/construct '~sig))]
              `((swap! t/expr-env assoc '~f (t/->Scheme (t/free ~sig) ~sig)))))
              sigs)
       ~@(map (fn [{:keys [f sig]}]
                (let [sig (parse/construct sig)
                      arglist (map vector (t/arglist sig) u/vars)]
                  ;; TODO: ^^ is this always an arrow sig?
                  `(defmulti
                     ~f
                     (fn ~(mapv second arglist)
                       ~(->> arglist
                             (filter (comp #{(first constrained)} first))
                             (mapv (fn [[_ x]] `(type ~x))))))))
              sigs)
       ~@(map (fn [{:keys [f args expr]}]
                `(defmethod ~f :default ~args ~expr))
              ;; TODO: ^^ needs type checking
              impls)
       ~constraint)))

(class Eq a
  (=    a -> a -> Bool)
  (not= a -> a -> Bool)

  (=    [x y] (not (not= x y)))
  (not= [x y] (not (= x y))))

(type/check (= 1 2))
;;; TODO: implement Substitutable for Constrained

(defmacro instance
  {:style/indent [2 1]}
  [& decl]
  `['~decl])

(instance Eq Int
  (== [x y] (c/= x y)))

(instance Eq Double
  (== [x y] (c/= x y)))

;;; can we do static dispatch?
(c/defn ==$Int [^long x ^long y]
  (if (c/= x y) True False))
(==$Int 1 1)

(tdef not Bool -> Bool)
(defn not [x] (if (= x True) False True))
;;; TODO: pattern matching `case` & `defn` macros
;;; TODO: exhaustiveness checking

;;; TODO: parse type constraints in `tdef`
;; (tdef == Eq a => a -> a -> Bool)

(swap! t/type-env assoc 'Eq (t/Constraint 'Eq ['a]))
(tdef =    Eq a => a -> a -> Bool) ;=> Eq a => (a -> (a -> Bool))
(tdef not= Eq a => a -> a -> Bool) ;=> Eq a => (a -> (a -> Bool))

(defmulti  =    (fn [a b] [(type a) (type b)]))
(defmulti  not= (fn [a b] [(type a) (type b)]))
(defmethod =    :default [a b] (not (not= a b)))
(defmethod not= :default [a b] (not (= a b)))

(defmethod == [(T Int) (T Int)] [a b] (if (c/= a b) True False))


(!= 1 1)


;; (instance Eq Nat)

;;; conflation
;;; type constraints
;;; parametric polymorphism
;;; ad-hoc polymorphism
;;; subtype polymorphism

;;; A thing is Eq if it has the functionality = or /=

(data Ordering = LT | EQ | GT)

(class Eq a => Ord a

  (compare a -> a -> Ordering)
  (compare [x y]
    (cond (= x y)  EQ
          (<= x y) LT
          :else    GT))

  (< a -> a -> Bool)
  (< [x y] (= LT (compare x y)))

  (<= a -> a -> Bool)
  (<= [x y] (not= GT (compare x y)))

  (> a -> a -> Bool)
  (> [x y] (= GT (compare x y)))

  (>= a -> a -> Bool)
  (>= [x y] (not= LT (compare x y)))

  (max a -> a -> a)
  (max [x y] (if (<= x y) y x))

  (min a -> a -> a)
  (min [x y] (if (<= x y) x y)))

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
