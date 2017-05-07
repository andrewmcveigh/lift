(ns tdd
  (:refer-clojure :exclude [even?])
  (:require
   [clojure.spec.alpha :as s]
   [lift.shorthand :refer [check sdef tdef] :as t]))

(sdef all-lengths, (string?) -> (nat-int?))
(tdef all-lengths [coll]
  (let [[x & xs] coll]
    (if x
      (lazy-seq (cons (count (first xs))
                      (all-lengths (rest xs))))
      ())))

(sdef xor, boolean? -> boolean? -> boolean?)
(tdef xor [a b]
  (cond (false? a) b
        (true? a) (not b)))

(sdef even?, nat-int? -> boolean?)
(tdef even? [x]
  (zero? (mod x 2)))

(sdef all-lengths, t/vect? n string? -> t/vect? n nat-int?)
(tdef all-lengths [xs] (mapv count xs))

;; (sdef ins-sort, t/vect? n )
