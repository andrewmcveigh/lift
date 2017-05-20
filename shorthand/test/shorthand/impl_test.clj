(ns lift.shorthand.impl-test
  (:refer-clojure :exclude [read])
  (:require
   [clojure.test :refer [deftest is testing]]
   [lift.shorthand.impl :refer :all]))


(deftest simple-parse-test

  (is (= (Type) (parse 'type?)))

  (is (= (Unit) (parse ())))

  (is (= (Var 'a) (parse 'a)))

  (is (= (Predicate 'int?)
         (parse 'int?)))

  (is (= (Tuple [(Predicate 'int?) (Predicate 'int?)])
         (parse '(int? * int?))))

  (is (= (Tuple [(Predicate 'int?) (Var 'a)])
         (parse '(int? * a))))

  (is (= (Tuple [(Var 'a) (Var 'b)])
         (parse '(a * b))))

  (is (= (Tuple [(Var 'a) (Var 'b) (Var 'c) (Predicate 'int?)])
         (parse '(a * b * c * int?))))

  (is (= (List (Predicate 'int?))
         (parse '[int?])))

  (is (= (List (Var 'a))
         (parse '[a])))

  (is (= (Param (Predicate 'maybe?) [(Var 'a)])
         (parse '(maybe? a))))

  (is (= (Param (Predicate 'either?) [(Var 'a) (Var 'b)])
         (parse '(either? a b))))

  (is (= (Value 1) (parse 1)))

  (is (= (Expr '+ [(Var 'n) (Value 1)])
         (parse '(+ n 1)))))


(deftest nested-parse-test

  (is (= (Tuple [(Param (Predicate 'maybe?) [(Var 'a)])
                 (Param (Predicate 'maybe?) [(Var 'b)])])
         (parse '(maybe? a * maybe? b))))

  (is (= (Tuple [(Param (Predicate 'maybe?) [(Var 'a)])
                 (Param (Predicate 'maybe?) [(Var 'b)])
                 (List (Var 'c))])
         (parse '(maybe? a * maybe? b * [c]))))

  (is (= (Tuple [(Param (Predicate 'maybe?) [(Var 'a)])
                 (Param (Predicate 'maybe?) [(Var 'b)])
                 (List (Tuple [(Param (Predicate 'either?)
                                      [(Var 'a) (Var 'b)])
                               (List (Var 'c))]))])
         (parse '(maybe? a * maybe? b * [(either? a b * [c])]))))

  (is (= (Param (Predicate 'vect?)
                [(Expr '+ [(Var 'n) (Value 1)]) (Predicate 'int?)])
         (parse '(vect? (+ n 1) int?))))

  (is (= (Param (Predicate 'vect?) [(Expr '+ [(Var 'n) (Value 1)]) (Var 'a)])
         (parse '(vect? (+ n 1) a))))

  (is (= (Param (Predicate 'either?)
                [(Var 'a)
                 (Param (Predicate 'either?)
                        [(Var 'b)
                         (Tuple [(Var 'a)
                                 (Param (Predicate 'maybe?) [(Var 'b)])])])])
         (parse '(either? a (either? b (a * (maybe? b))))))))


(deftest function-parse-test

  (is (= (Function [(Predicate 'int?)] (Predicate 'int?))
         (parse '(int? -> int?))))

  (is (= (Function [(Var 'a)] (Var 'b))
         (parse '(a -> b))))

  (is (= (Function [(Var 'a) (Var 'b)] (Var 'c))
         (parse '(a -> b -> c))))

  (is (= (Function [(List (Var 'a))] (Param (Predicate 'maybe?) [(Var 'a)]))
         (parse '([a] -> maybe? a))))

  (is (= (Function [(Param (Predicate 'vect?) [(Var 'n) (Var 'a)]) (Var 'a)]
                   (Param (Predicate 'vect?)
                          [(Expr '+ [(Var 'n) (Value 1)]) (Var 'a)]))
         (parse '(vect? n a -> a -> vect? (+ n 1) a))))

  (is (= (Function [(Var 'a)
                    (Function [(Var 'b) (Var 'c)]
                              (Param (Predicate 'either?)
                                     [(Var 'a)
                                      (Function [(Var 'b)]
                                                (Param (Predicate 'maybe?)
                                                       [(Var 'a)]))]))]
                   (Param (Predicate 'maybe?) [(Var 'a)]))
         (parse '(a -> (b -> c -> (either? a (b -> maybe? a))) -> maybe? a))))

  (is (= (Function [(Param (Predicate 'list?) [(Var 'a)])
                    (Param (Predicate 'list?) [(Var 'b)])
                    (Function [(Var 'a) (Var 'b)] (Var 'c))]
                   (Param (Predicate 'list?) [(Var 'c)]))
         (parse '(list? a -> list? b -> (a -> b -> c) -> list? c)))))


;; (clojure.test/run-tests)
