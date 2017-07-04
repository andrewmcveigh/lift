(ns lift.type.check
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [lift.f.functor :as f :refer [Functor]]
   [lift.type.substitution :as sub :refer [sub Substitutable]]
   [lift.type.syntax :as syn]
   [lift.type.type :as t :refer [Free free]]
   [lift.type.util :as u])
  (:import
   [lift.type.substitution Sub]
   [lift.type.type Scheme]
   [lift.type.type.types
    Unit Const Var Arrow Product Sum Constraint Constrained]))

(defrecord Env []
  Functor
  (-map [x f]
    (map->Env (f/map f (into {} x)))))

(extend-protocol Free
  Unit
  (free [_] #{})
  Const
  (free [_] #{})
  Var
  (free [x] #{(.v x)})
  Arrow
  (free [x]
    (set/union (or (some-> (.a x) free) #{}) (or (some-> (.b x) free) #{})))
  Product
  (free [x]
    (apply set/union (map free (.vars x))))
  Sum
  (free [x]
    (apply set/union (map free (.vars x))))
  Constraint
  (free [x]
    (apply set/union (map free (.vars x))))
  Constrained
  (free [x]
    (free (.type x)))
  Env
  (free [x] (set (map free (vals x))))
  Scheme
  (free [{:keys [t vars]}]
    (set/difference (free t) (set vars))))

(extend-protocol Substitutable
  Unit
  (sub [x _] x)
  Const
  (sub [x _] x)
  Var
  (sub [x subst]
    (get subst (.v x) x))
  Arrow
  (sub [x subst]
    (t/Arrow (sub (.a x) subst) (sub (.b x) subst)))
  Product
  (sub [x subst]
    (t/Product (.tag x) (map #(sub % subst) (.vars x))))
  Sum
  (sub [x subst]
    (t/Sum (.tag x) (map #(sub % subst) (.vars x))))
  Constraint
  (sub [x subst]
    (t/Constraint (.tag x) (mapv #(sub % subst) (.vars x))))
  Constrained
  (sub [x subst]
    (let [constraint (.constraint x)
          ctag (.tag constraint)
          [cvar] (.vars constraint)]
      (t/Constrained (t/Constraint ctag [(sub cvar subst)])
                     (sub (.type x) subst))))
  Sub
  (sub [x subst]
    (f/map (fn [[term var :as s]]
             (if (empty? s)
               []
               [(sub term subst) var]))
           x))
  Env
  (sub [x subst]
    (f/map #(sub % subst) x))
  Scheme
  (sub [{:keys [vars t] :as x} subst]
    (update x :t sub (apply dissoc subst vars))))

(defn occurs?
  "A variable `x` occurs in `term` if and only if `t = f(s[1],...s[n])` for
  `n > 0` and either `s[i] = x` or `x` occurs in `s[i]` for some
  `i = 1,2,...,n`"
  [x term]
  (contains? (free term) x))

(defmulti unify
  (fn [t u] [(type t) (type u)]))

(defn ex-infitite-type [a t]
  (throw
   (Exception. (format "Infinite Type %s %s" (pr-str a) (pr-str t)))))

(defn ex-cannot-unify [a b]
  (throw
   (ex-info (format "Cannot unify %s and %s" (pr-str a) (pr-str b))
            {:type ::ex-cannot-unify
             :a a
             :b b})))

(defn ex-arity [a b]
  (throw
   (Exception.
    (format "Arities do not match %s %s" (pr-str a) (pr-str b)))))

(defn ex-unbound-var [v]
  (throw (Exception. (format "UnboundVariable %s" (pr-str v)))))

(defn ex-undeclared-instance [constraint]
  (throw
   (Exception. (format "There is no %s instance defined" (pr-str constraint)))))

(defn bind [a t]
  (cond (= t (t/Var a)) sub/id
        (occurs? a t)   (ex-infitite-type a t)
        :otherwise      (sub/singleton a t)))

(defn unify-seq [t1 t2]
  (let [ts1 (.vars t1)
        ts2 (.vars t2)]
    (if (= (count ts1) (count ts2))
      (loop [s1 sub/id ts1 ts1 ts2 ts2]
        (if (and (empty? ts1) (empty? ts2))
          s1
          (let [[t1 & t1s] ts1
                [t2 & t2s] ts2
                s2 (trampoline unify t1 t2)]
            (recur (sub/compose s2 s1) t1s t2s))))
      (ex-arity t1 t2))))

(defn unify-pair [t1 t2]
  (if (= t1 t2)
    sub/id
    (let [s1 (trampoline unify (.a t1) (.a t2))
          s2 (trampoline unify (sub (.b t1) s1) (sub (.b t2) s1))]
      (sub/compose s2 s1))))

(defmethod unify [Const Const] [a b]
  (if (= a b)
    sub/id
    (ex-cannot-unify a b)))

(defmethod unify [Const Var]
  [t1 t2]
  (bind (.v t2) t1))

(defmethod unify [Var Const]
  [t1 t2]
  (bind (.v t1) t2))

(defmethod unify [Var Var]
  [t1 t2]
  (bind (.v t2) t1))

(defmethod unify [Var Arrow]
  [t1 t2]
  (bind (.v t1) t2))

(defmethod unify [Arrow Var]
  [t1 t2]
  (bind (.v t2) t1))

(defmethod unify [Arrow Arrow]
  [t1 t2]
  (unify-pair t1 t2))

(defmethod unify [Product Product]
  [t1 t2]
  (unify-seq t1 t2))

(defmethod unify [Product Var]
  [t1 t2]
  (bind (.v t2) t1))

(defmethod unify [Var Product]
  [t1 t2]
  (bind (.v t1) t2))

(defmethod unify [Sum Sum]
  [t1 t2]
  (unify-seq t1 t2))

(defmethod unify [Sum Var]
  [t1 t2]
  (bind (.v t2) t1))

(defmethod unify [Var Sum]
  [t1 t2]
  (bind (.v t1) t2))

(defmethod unify [Constrained Arrow]
  [t1 t2]
  (let [subst (trampoline unify (.type t1) t2)
        concrete (sub (.constraint t1) subst)]
    (if (t/instance? concrete)
      subst
      (ex-undeclared-instance concrete))))

(defmethod unify [Arrow Constrained]
  [t1 t2]
  (trampoline unify t2 t1))

(defmethod unify :default [a b]
  (ex-cannot-unify a b))


(let [fresh-vars (atom (for [a (map char (range 97 123))
                             i (rest (range))]
                         (symbol (str a i))))]
  (defn fresh []
    (if-let [v (first @fresh-vars)]
      (do
        (swap! fresh-vars rest)
        (t/Var v))
      (throw (Exception. "No more fresh vars left!")))))

(defn instantiate [scheme]
  (let [vars  (:vars scheme)
        vars' (map (fn [_] (fresh)) vars)
        subst (Sub. (mapv vector vars vars'))]
    (sub (:t scheme) subst)))

(defn generalize [env t]
  (Scheme. (set/difference (free t) (free env)) t))

(defn infer [env [syn-type expr]]
  (case syn-type

    ::syn/Lit [sub/id (t/Const expr)]

    ::syn/Var (if-let [s (or (get env expr)
                             (get env (u/resolve-sym expr)))]
                [sub/id (instantiate s)]
                (ex-unbound-var expr))

    ::syn/Lam (let [[x e]   expr
                    tv      (fresh)
                    env'    (assoc env x (Scheme. [] tv))
                    [s1 t1] (infer env' e)]
                [s1 (sub (t/Arrow tv t1) s1)])

    ::syn/App (let [[e1 e2] expr
                    tv      (fresh)
                    [s1 t1] (infer env e1)
                    [s2 t2] (infer (sub env s1) e2)
                    s3      (unify (sub t1 s2)
                                   (t/Arrow t2 tv))]
                [(sub/compose s3 s2 s1) (sub tv s3)])

    ::syn/Let (let [[x e1 e2] expr
                    [s1 t1]   (infer env e1)
                    env'      (sub env s1)
                    t         (generalize env' t1)
                    [s2 t2]   (infer (assoc env' x t) e2)]
                [(sub/compose s1 s2) t2])

    ::syn/If  (let [[cond then else] expr
                    [s1 t1] (infer env cond)
                    [s2 t2] (infer env then)
                    [s3 t3] (infer env else)
                    s4      (unify t1 (t/Sum 'Bool []))
                    s5      (unify t2 t3)]
                [(sub/compose s5 s4 s3 s2 s1) (sub t2 s5)])))
