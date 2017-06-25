(ns lift.t.check-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [lift.t.check :refer [unify]]
   [lift.t.substitution :as sub]
   [lift.t.type :as t]))

(def Int (t/Const 'int?))
(def Bool (t/Const 'bool?))

(def a (t/Var 'a))
(def b (t/Var 'b))
(def c (t/Var 'c))

(def f (t/Arrow a b))
(def g (t/Arrow b c))

(defn forward-back-unify [a b]
  [(let [mgu (unify a b)]
     [(sub/sub a mgu) (sub/sub b mgu)])
   (let [mgu (unify b a)]
     [(sub/sub a mgu) (sub/sub b mgu)])])

(defn is-unifiable [a b]
  (let [[[a1 b1] [a2 b2]] (forward-back-unify a b)]
    (is (= a1 b1))
    (is (= a2 b2))))

(defn un-unifiable [a b]
  (let [[[a1 b1] [a2 b2]] (forward-back-unify a b)]
    (is (not= a1 b1))
    (is (not= a2 b2))))

(deftest unify-test
  ;; (un-unifiable (t/Const 'boolean?) (t/Const 'int?))
  (is-unifiable (t/Var 'a) (t/Const 'int?))
  (is-unifiable (t/Var 'a) (t/Var 'y))
  (is-unifiable (t/Arrow (t/Var 'a) (t/Var 'y))
                (t/Arrow (t/Var 'x) (t/Arrow (t/Var 'b) (t/Var 'x))))
  (is-unifiable (t/Product (t/Var 'a) (t/Const 'int?))
                (t/Product (t/Const 'int?) (t/Var 'b)))
  (is-unifiable (t/Product (t/Var 'a) (t/Const 'int?))
                (t/Product (t/Var 'b) (t/Const 'int?)))
  (un-unifiable (t/Product (t/Var 'a) (t/Const 'int?))
                (t/Product (t/Var 'b) (t/Var 'a)))
  (is-unifiable (t/Product (t/Var 'a) (t/Const 'int?))
                (t/Product (t/Var 'a) (t/Var 'b)))
  (is-unifiable (t/Sum (t/Var 'a) (t/Const 'int?))
                (t/Sum (t/Var 'a) (t/Var 'b)))
  )

(comment

  (clojure.test/run-tests)

  )
