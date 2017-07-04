(ns lift.type.prelude
  (:refer-clojure :exclude [= class not])
  (:require
   [lift.type.core :refer [check class const data defn instance tdef T]]
   [lift.type.util :as u]
   [lift.type.type :as t]
   [lift.type.syntax :as syn]))

(alias 'c 'clojure.core)

(const Char char?)
(const Int integer?)
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

;; (tdef quote a -> a)
;; TODO: ^^

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

;; (check (= 1.0 0.3))

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

;;; TODO: Constrained constraints

;; (methods =)
;; => {[Int Int] #function[lift.type.prelude/eval24741/fn--24742]}

;; (keys @lift.type.type/expr-env)

;; (reset! lift.type.type/expr-env {})

;; (check (= 1 2))

;;; what does it mean to unify a constrained type?
;;; * the constraints must match
;;;   - if Eq a => a then any attempt to unify, there must be an instance of Eq
;;;     for a
;;; which should be easy enough, but there are a couple of trick parts.
;;; * can we check if this instance exists?

;;; (instance Eq Int
;;;   (= [x y] (c/= x y)))
;;;=> (defmethod = [Int Int] [x y] (c/= x y))
