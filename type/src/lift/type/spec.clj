(ns lift.type.spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::unit #{()})

(s/def ::type-name
  (s/and simple-symbol? #(re-matches #"^[A-Z][A-z]+$" (name %))))

(s/def ::type-var
  (s/and simple-symbol? #(re-matches #"^[a-z\-]+$" (name %))))

(s/def ::arrow-regex
  (s/cat :type ::retype :more (s/+ (s/cat :-> #{'->} :type ::retype))))

(s/def ::arrow-parens
  (s/and seq? ::arrow-regex))

(s/def ::arrow
  (s/alt :noparens ::arrow-regex
         :parens   ::arrow-parens))

(s/def ::parameterized-regex
  (s/cat :type-name ::type-name :args (s/+ ::type)))

(s/def ::parameterized
  (s/alt :parens   (s/and seq? ::parameterized-regex)
         :noparens ::parameterized-regex))

(s/def ::resubsig
  (s/alt :sub-arrow ::arrow :retype ::retype))

(s/def ::constrained
  (s/cat :constraint (s/cat :constraint ::parameterized :=> #{'=>})
         :sig ::resubsig))

(s/def ::sig
  (s/or :constrained ::constrained
        :non-constrained ::type))

(s/def ::resig
  (s/alt :constrained ::constrained
         :non-constrained ::retype))

(s/def ::type
  (s/or :unit          ::unit
        :type-name     ::type-name
        :type-var      ::type-var
        :arrow         ::arrow-parens
        :parameterized ::parameterized))

(s/def ::retype
  (s/alt :unit          ::unit
         :type-name     ::type-name
         :type-var      ::type-var
         :arrow         ::arrow-parens
         :parameterized ::parameterized))

(s/def ::product
  (s/cat :type-cons ::type-cons := #{'=} :value-cons ::parameterized))

(s/def ::parameterized-type-cons
  (s/cat :type-name ::type-name :type-params (s/+ ::type-var)))

(s/def ::type-cons
  (s/alt :lit-type-cons ::type-name
         :par-type-cons ::parameterized-type-cons))

(s/def ::sum-cons
  (s/cat :type ::retype
         :more (s/+ (s/cat :| #{'|} :type ::retype))))

(s/def ::sum
  (s/cat :type-cons ::type-cons := #{'=} :sum-cons ::sum-cons))

(s/def ::data
  (s/or :product ::product :sum ::sum))

(s/def ::impl
  (s/and seq?
         (s/cat :f simple-symbol?
                :args (s/coll-of simple-symbol? :kind vector?)
                :expr any?)))

(s/def ::class
  (s/cat :class ::parameterized
         :sigs  (s/+ (s/and seq? (s/cat :f simple-symbol? :sig ::resubsig)))
         :impls (s/* ::impl)))
