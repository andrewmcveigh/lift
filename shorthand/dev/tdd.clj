(ns tdd
  (:refer-clojure :exclude [defn even?])
  (:require
   [clojure.spec.alpha :as s]
   [lift.shorthand :refer [check defn sdef tdef] :as t]))

(sdef all-lengths, (string?) -> (nat-int?))
(defn all-lengths [coll]
  (let [[x & xs] coll]
    (if x
      (lazy-seq (cons (count (first xs))
                      (all-lengths (rest xs))))
      ())))

(sdef xor, boolean? -> boolean? -> boolean?)
(defn xor [a b]
  (cond (false? a) b
        (true? a) (not b)))

(sdef even?, nat-int? -> boolean?)
(defn even? [x]
  (zero? (mod x 2)))

(sdef all-lengths, t/vect? n string? -> t/vect? n nat-int?)
(defn all-lengths [xs] (mapv count xs))

(sdef ins-sort, Ord a => t/vect? n a -> t/vect? n a)
