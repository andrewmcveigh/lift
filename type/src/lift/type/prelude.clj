(ns lift.type.prelude
  (:refer-clojure :exclude [= class defn not not=])
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

(instance Ord String
  (compare [x y]
    (let [z (c/compare x y)] (cond (zero? z) EQ (neg? z) LT :else GT))))

;; (check (min "" 1))

;; (c/compare "bb" "d")

;;; TODO:
;; (check (min 1 "")) ;=> Cannot unify Int and String
;; (check (min "" 1)) ;=> There is no Ord String instance defined

;;; TODO: Constrained to Var unify
