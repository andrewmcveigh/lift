(ns lift.type.syntax
  (:require
   [clojure.spec.alpha :as s]
   [lift.type.parse :refer [data]]
   [lift.type.type :as t]))

(def lits (atom {}))

(defmacro def-literal [type parser]
  (let [litmap (assoc @lits (keyword (name type)) [parser type])
        ormap  (map (fn [[k [s]]] [k s]) litmap)
        summap (map (fn [[_ [_ t]]] [(symbol (str \L (name t))) t]) litmap)]
    `(do
       (reset! lits '~litmap)
       (s/def ::literal
         (s/or ~@(apply concat ormap)))
       (data ~'Lit ~'= ~@(apply concat (interpose '[|] summap))))))

(defmacro const [name parser]
  `(let [t# (t/Const '~name)]
     (swap! t/type-env assoc '~name t#)
     (def-literal ~name ~parser)
     t#))

(const Char   char?)
(const Int    integer?)
(const Double double?)
(const String string?)

(data Symbol = Symbol String)
(data List a = Nil | Cons a (List a))

(data Expr
  = Sym Symbol
  | Lit Lit
  | Quo Expr
  | Seq List
  | App Expr Expr
  | Lam Symbol Expr
  | Let Symbol Expr Expr
  | If  Expr Expr Expr)

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

(s/def ::qvec
  (s/coll-of ::quoted :kind vector?))

(s/def ::seq
  (s/coll-of ::quoted :kind seq?))

(s/def ::quoted
  (s/or ::Lit ::literal
        ::Var ::var
        ::Seq ::seq
        ::Vec ::qvec))

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
    (recur (App op (first args)) (rest args))
    op))

(defmulti parse* (fn [[t _]] t))

(defmethod parse* ::Lit [[_ node]]
  (Lit (->> node first (get @lits) second)))

(defmethod parse* ::Var [[_ node]]
  (Sym node))

(defmethod parse* ::Lam [[_ node]]
  (Lam (first (::bind node)) (parse* (::expr node))))

(defmethod parse* ::App [[_ node]]
  (curry (parse* (::op node)) (map parse* (::args node))))

(defmethod parse* ::Let [[_ node]]
  (Let (first (::bind node))
       (parse* (second (::bind node)))
       (parse* (::expr node))))

(defmethod parse* ::If  [[_ node]]
  (If (parse* (::cond node))
      (parse* (::then node))
      (parse* (::else node))))

(defmethod parse* ::Quo [[_ node]]
  (Quo (parse* (::expr node))))
;; Quote means don't eval. What does it mean to types?

(defmethod parse* ::Seq [[_ node]]
  (Seq (map parse* node)))

(defmethod parse* ::Vec [[_ node]]
  (curry (Sym 'VCons) (conj (mapv parse* node) (Sym 'VNil))))

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
      (parse* ast))))

(parse ''(+ 1 2))
