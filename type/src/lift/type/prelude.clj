(ns lift.type.prelude
  (:refer-clojure :exclude [= class defn not not=])
  (:require
   [lift.p.pattern :as p]
   [lift.type.core :refer [check class const data defn instance tdef T]]
   [lift.type.util :as u]
   [lift.type.type :as t]
   [lift.type.syntax :as syn]))

(alias 'c 'clojure.core)

(const Char char?)
(const Int integer?)
(const Nat nat-int?)
(const Double double?)
(const String string?)

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
(tdef dec Int -> Int)

(data List a = Nil | Cons a (List a))

(data Vector a = VNil | VCons a (Vector a))

(tdef map (a -> b) -> (List a) -> (List b))

(tdef not Bool -> Bool)
(c/defn not [x] (if (c/= x True) False True))

(class Eq a
  (=    a -> a -> Bool)
  (not= a -> a -> Bool)

  (=    [x y] (not (not= x y)))
  (not= [x y] (not (= x y))))

(instance Eq Int
  (= [x y] (if (c/= x y) True False)))

(instance Eq Double
  (= [x y] (if (c/= x y) True False)))

(data Ordering = LT | EQ | GT)

(instance Eq Ordering
  (= [x y] (if (c/= x y) True False)))

(instance Eq String
  (= [x y] (if (c/= x y) True False)))

(class Eq a => Ord a
  (compare a -> a -> Ordering)
  (<       a -> a -> Bool)
  (<=      a -> a -> Bool)
  (>       a -> a -> Bool)
  (>=      a -> a -> Bool)
  (max     a -> a -> a)
  (min     a -> a -> a)

  (compare [x y] (cond (c/= x y) EQ (c/<= x y) LT :else GT))
  (<       [x y] (= LT (compare x y)))
  (<=      [x y] (not= GT (compare x y)))
  (>       [x y] (= GT (compare x y)))
  (>=      [x y] (not= LT (compare x y)))
  (max     [x y] (if (<= x y) y x))
  (min     [x y] (if (<= x y) x y)))

(instance Ord Int
  (<= [x y] (if (c/<= x y) True False)))

(instance Ord Double
  (<= [x y] (if (c/<= x y) True False)))

(instance Ord String
  (compare [x y]
    (let [z (c/compare x y)] (cond (zero? z) EQ (neg? z) LT :else GT))))

(class Ord a => Num a
  (+ a -> a -> a)
  (- a -> a -> a)
  (* a -> a -> a)

  (+ [x y] (c/+ x y))
  (- [x y] (c/- x y))
  (* [x y] (c/* x y)))

(instance Num Int)
(instance Num Double)

;; (p/case True
;;   True 1
;;   False 0)

(data Symbol = Symbol String)

(data Expr
  = Var Symbol
  | App Expr Expr
  | Lam Symbol Expr
  | Let Symbol Expr Expr
  | Lit Lit
  | If Expr Expr Expr)

(check '(if True 1 2))

(check '(1 2 3))

(tdef def Expr -> Expr)


(syn/parse '(def x 1))

;; Should syntax know about special forms? My first thoughts are no, but then
;; should syntax be re-parsed when there's a macro?
;; should syntax be first parsed into seqs? - probably, that's more like how
;; lisps work

;; TODO: special forms need to have special syntax parser/rules
;; TODO: macros receive Exprs
;; TODO: type checker/system macros?

;; (tdef quote a -> a)
;; TODO: ^^ quote is a reader macro/special form
;; TODO: extensible parser/rules


;; (check (Lam (Symbol "a")
;;             (App (Var (Symbol "+"))
;;                  (Lit (LInt 1)))))

;; (check (Lit (LInt 1)))

;; (data Point = Point {:x Int :y Int})
;; TODO: records^^

;; [:record
;;  {:type-cons [:lit-type-cons Point],
;;   := =,
;;   :rec-cons
;;   {:type-name Point,
;;    :recmap
;;    {:x
;;     [:parameterized
;;      [:noparens {:type-name Just, :args [[:type-var a]]}]],
;;     :y [:type-name Int]}}}]

;; 2 versions of the constructor, one accepting a map, one accepting 2 args
;; 2 ways of destructuring
;; The problem is that map syntax ordering is weak
;; (data Point = Point Int Int)

;;; hash-map/array-map reading could be hacked
