(ns lift.type.syntax
  (:require
   [clojure.spec.alpha :as s]))

(s/def ::literal
  (s/or ::boolean? boolean?
        ::char?    char?
        ::string?  string?
        ::integer? integer?
        ::double?  double?
        ::decimal? decimal?
        ::keyword? keyword?))

(def literal-map
  '{::boolean? Bool
    ::char?    Char
    ::string?  String
    ::integer? Int
    ::double?  Double
    ::decimal? Decimal
    ::keyword? Keyword})

(s/def ::var symbol?)

(s/def ::lambda
  (s/and seq?
         (s/cat ::lamb #{'fn}
                ::bind (s/coll-of ::var :kind vector?)
                ::expr ::expr)))

(s/def ::application
  (s/and seq?
         (s/cat ::op ::expr ::args (s/* ::expr))))

(s/def ::let
  (s/and seq?
         (s/cat ::let #{'let}
                ::bind (s/tuple ::var ::expr)
                ::expr ::expr)))

(s/def ::if
  (s/and seq?
         (s/cat ::if #{'if}
                ::cond ::expr
                ::then ::expr
                ::else ::expr)))

(s/def ::vector
  (s/coll-of ::expr :kind vector?))

(s/def ::expr
  (s/or ::Lit ::literal
        ::Var ::var
        ::Lam ::lambda
        ::Let ::let
        ::App ::application
        ::If  ::if
        ::Vec ::vector))

(defn curry [op args]
  (if (seq args)
    (recur [::App [op (first args)]] (rest args))
    op))

(defn normalize [[syn-type node]]
  (case syn-type
    ::Lit [::Lit (get literal-map (first node))]
    ::Var [::Var node]
    ::Lam [::Lam [(first (::bind node))
                  (normalize (::expr node))]]
    ::App (curry (normalize (::op node))
                 (map normalize (::args node)))
    ::Let [::Let [(first (::bind node))
                  (normalize (second (::bind node)))
                  (normalize (::expr node))]]
    ::If  [::If [(normalize (::cond node))
                 (normalize (::then node))
                 (normalize (::else node))]]
    ::Vec (curry [::Var 'VCons]
                 (conj (mapv normalize node) [::Var 'VNil]))))

(defn parse [expr]
  (let [ast (s/conform ::expr expr)]
    (if (s/invalid? ast)
      (do
        (s/explain ::expr expr)
        (throw (Exception. "Invalid Syntax")))
      (normalize ast))))


;;; TODO: How to elaborate syntax such as [a] -> (vector a)
;;; TODO: How to varargs
