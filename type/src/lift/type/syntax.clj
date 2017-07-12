(ns lift.type.syntax
  (:require
   [clojure.spec.alpha :as s]))

(def lits (atom {}))

(defmacro def-literal [type parser]
  (let [litmap (assoc @lits (keyword (name type)) [parser type])
        ormap  (map (fn [[k [s]]] [k s]) litmap)]
    `(do
       (reset! lits '~litmap)
       (s/def ::literal
         (s/or ~@(apply concat ormap))))))

(def-literal Char   char?)
(def-literal Int    integer?)
(def-literal Double double?)
(def-literal String string?)

(s/def ::var symbol?)

(s/def ::lambda
  (s/and seq?
         (s/cat ::lamb #{'fn}
                ::bind (s/coll-of ::var :kind vector?)
                ::expr ::expr)))

(s/def ::application
  (s/and seq? (s/cat ::op ::expr ::args (s/+ ::expr))))

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

(s/def ::quote
  (s/and seq? (s/cat ::quot #{'quote} ::expr ::quoted)))

(s/def ::vector
  (s/coll-of ::expr :kind vector?))

(s/def ::seq
  (s/coll-of ::expr :kind seq?))

(s/def ::quoted
  (s/or ::Lit ::literal
        ::Var ::var
        ::Lam ::lambda
        ::Let ::let
        ::If  ::if
        ::Quo ::quote
        ::Seq ::seq
        ::Vec ::vector))

(s/def ::expr
  (s/or ::Lit ::literal
        ::Var ::var
        ::Lam ::lambda
        ::Let ::let
        ::If  ::if
        ::Quo ::quote
        ::App ::application
        ::Vec ::vector))

(defn curry [op args]
  (if (seq args)
    (recur [::App [op (first args)]] (rest args))
    op))

(defn normalize [[syn-type node]]
  (case syn-type
    ::Lit [::Lit (->> node first (get @lits) second)]
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
    ::Quo [::Quo (normalize (::expr node))]
    ::Seq (curry [::Var 'Cons]
                 (conj (mapv normalize node) [::Var 'Nil]))
    ::Vec (curry [::Var 'VCons]
                 (conj (mapv normalize node) [::Var 'VNil]))))

;; what does quote mean?
;; it means that the var type should not be looked up,
;; and expressions should not be evaluated
;; What about collection syntax expansion under quote?
;; Maybe it's better to build typed datastructures, so that we can be clear
;; where is syntax and where is ... how?
;; Maybe it's better to start dogfooding Expr anyway? Maybe more interesting?

;;; TODO: How to varargs
(defn parse [expr]
  (let [ast (s/conform ::expr expr)]
    (if (s/invalid? ast)
      (do
        (s/explain ::expr expr)
        (throw (Exception. "Invalid Syntax")))
      (normalize ast))))
