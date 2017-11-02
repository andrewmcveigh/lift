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

(tdef identity {x a} -> a)

(tdef comp [{f} (a -> b) -> & fs [(b -> c)]]  a -> c)

(tdef comp [{f (a -> b)} & {fs (b -> c)}] -> (a -> c))

(tdef comp
  ((a -> b) -> (& b -> c) -> (a -> c)))

(tdef comp
  ((a -> b) -> &(b+ -> c+) -> (a -> c+)))

(tdef apply
  ((& a+ -> b) -> & a+ -> [a+] -> b))

(tdef juxt
  ((a -> b) -> & (c+ -> d+) -> a -> [b c+ d+])
  )

(t/Arrow (t/Var 'a) (t/Var 'b))

(a* n -> b)

(& (a -> _) n) -> a -> (n-tuple n _)
&       : variadic args
_       : unlabeled type variable
n       : collect number of variadic args
n-tuple : tuple of size discerned by first arg
n       : match 1st n
_       : match unlabeled type variables

(tdef juxt
  ((a -> _)* n -> a -> [_* n]))

(tdef apply
  ((_* n -> b) -> _* (dec n) -> (List _) -> b))

(tdef comp
  ((_a -> _b)* -> (_a -> _b)))

(tdef apply
  (let [ftype (fn [n] (t/Arrow))] ((& (a+ n) -> b) -> & (a+ (dec n))))
  )


(infer {:ctx (t/map->Env {})}
       (syn/parse ''(if True (+ 1 1) (- 1 1))))

;;; TODO: Can't pass an Expr to a macro, because the Expr would need unwrapping
;;; could it be a type class? Are we just talking about dynamic typing for
;;; macros? Because there would be easier ways.
;;; you can only do that with functions whose return type is compatible with
;;; it's first argument
;;; How about variadics then?
;;; Can we, during/between syntax analysis & type-checking also check arities?
;;; are variadic args easier to express when you have type-level Nat, and the
;;; ability to make them implicit?

;;; `&`: variadic
;;; `[a-z]\+`: iterate through fresh type-vars that are not already taken
;;; `a+` the first instance of var `a+` starts a matching cycle with other `a+`s
;;; I.E., if the 1st `a+` matched a 5 arg fn we'd match:
;;;   `a0 -> a1 -> a2 -> a3 -> a4` and the subsequent `a+`s would be forced to be
;;;   `a0 -> a1 -> a2 -> a3 -> [a4]`

;;; there's a difference between (& b -> c) and (& next -> n+1)

;; Should syntax know about special forms? My first thoughts are no, but then
;; should syntax be re-parsed when there's a macro?
;; should syntax be first parsed into seqs? - probably, that's more like how
;; lisps work

;; TODO: special forms need to have special syntax parser/rules
;; TODO: macros receive Exprs - nope
;; TODO: type checker/system macros?
;; TODO: extensible parser/rules
;; TODO: records^^
;; TODO: type-system introspection - type errors at runtime
;; TODO: variadic fn type syntax
;; TODO: variadic fn type checking
;; TODO: What things are really important to a lisp, that we can't ditch
