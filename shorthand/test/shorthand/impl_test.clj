(ns lift.shorthand.impl-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [lift.shorthand.impl :refer :all]))

(deftest simple-parse-test

  (testing "Can parse `type?`"
    (let [sig (parse 'type?)]
      (is (= (Type) sig))))

  (testing "Can parse `()`"
    (let [sig (parse ())]
      (is (= (Unit) sig))))

  (testing "Can parse `a`"
    (let [sig (parse 'a)]
      (is (= (Var 'a) sig))))

  (testing "Can parse `int?`"
    (let [sig (parse 'int?)]
      (is (= (Predicate 'int?) sig))))

  (testing "Can parse `(int? * int?)`"
    (let [sig (parse '(int? * int?))]
      (is (= (Tuple [(Predicate 'int?) (Predicate 'int?)]) sig))))

  (testing "Can parse `(int? * a)`"
    (let [sig (parse '(int? * a))]
      (is (= (Tuple [(Predicate 'int?) (Var 'a)]) sig))))

  (testing "Can parse `(a * b)`"
    (let [sig (parse '(a * b))]
      (is (= (Tuple [(Var 'a) (Var 'b)]) sig))))

  (testing "Can parse `[int?]`"
    (let [sig (parse '[int?])]
      (is (= (List (Predicate 'int?)) sig))))

  (testing "Can parse `[a]`"
    (let [sig (parse '[a])]
      (is (= (List (Var 'a)) sig))))

  (testing "Can parse `maybe? a`"
    (let [sig (parse '(maybe? a))]
      (is (= (Param (Predicate 'maybe?) [(Var 'a)]) sig))))

  (testing "Can parse `either? a b`"
    (let [sig (parse '(either? a b))]
      (is (= (Param (Predicate 'either?) [(Var 'a) (Var 'b)]) sig))))

  (testing "Can parse `1`"
    (let [sig (parse 1)]
      (is (= (Value 1) sig))))

  (testing "Can parse `(+ n 1)`"
    (let [sig (parse '(+ n 1))]
      (is (= (Expr '+ [(Var 'n) (Value 1)]) sig))))

  (testing "Can parse `vect? (+ n 1) int?`"
    (let [sig (parse '(vect? (+ n 1) int?))]
      (is (= (Param (Predicate 'vect?)
                    [(Expr '+ [(Var 'n) (Value 1)]) (Predicate 'int?)]) sig))))

  (testing "Can parse `vect? (+ n 1) a`"
    (let [sig (parse '(vect? (+ n 1) a))]
      (is (= (Param (Predicate 'vect?)
                    [(Expr '+ [(Var 'n) (Value 1)]) (Var 'a)]) sig))))

  )

;; (parse '(list? a -> list? b -> (a -> b -> c) -> list? c))

(clojure.test/run-tests)
