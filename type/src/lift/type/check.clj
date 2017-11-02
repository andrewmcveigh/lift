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
   [lift.type.syntax.types Lit Seq Sym Lam Let If Quo App]
   [lift.type.type Env Scheme]
   [lift.type.type.types
    Unit Const Var Arrow Product Sum Constraint Constrained]))

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

(defn unify-tuples [t1 t2]
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

(defn unify-seq [seq-term]
  (loop [s1 sub/id
         [t1 t2 & ts] seq-term]
    (if (and t1 t2)
      (let [s2 (trampoline unify t1 t2)
            t3 (sub/sub t1 s2)]
        (recur (sub/compose s2 s1) (cons t3 ts)))
      s1)))
;; TODO: does this work?

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
  (unify-tuples t1 t2))

(defmethod unify [Product Var]
  [t1 t2]
  (bind (.v t2) t1))

(defmethod unify [Var Product]
  [t1 t2]
  (bind (.v t1) t2))

(defmethod unify [Sum Sum]
  [t1 t2]
  (unify-tuples t1 t2))

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

(defmulti infer (fn [_ expr] (class expr)))

(defmethod infer Lit [{:keys [quoted?]} expr]
  (if quoted?
    [sub/id (t/Sum 'Expr [])]
    [sub/id (t/Const (.-a expr))]))

(defmethod infer Sym [{:keys [quoted? ctx]} expr]
  (if quoted?
    [sub/id (t/Sum 'Expr [])]
    (if-let [s (or (get ctx expr)
                   (get ctx (u/resolve-sym (.-a expr))))]
      [sub/id (instantiate s)]
      (ex-unbound-var expr))))

(defmethod infer Lam [env expr]
  (let [[x e]   (t/destructure expr)
        tv      (fresh)
        [s1 t1] (-> env
                    (assoc-in [:ctx x] (Scheme. [] tv))
                    (infer e))]
    [s1 (sub (t/Arrow tv t1) s1)]))

(defmethod infer App [env expr]
  (let [[e1 e2] (t/destructure expr)
        ;; TODO: If e1 is a macro, should we "simply" switch the type-checking
        ;; rules here? How do we even know if e1 is a macro or not? That means
        ;; we'd have to use the clojure environment.
        tv      (fresh)
        [s1 t1] (infer env e1)
        [s2 t2] (infer (update env :ctx sub s1) e2)
        s3      (unify (sub t1 s2) (t/Arrow t2 tv))]
    [(sub/compose s3 s2 s1) (sub tv s3)]))

(defmethod infer Let [{:keys [env]} expr]
  (let [[x e1 e2] (t/destructure expr)
        [s1 t1]   (infer env e1)
        env'      (update env :ctx sub s1)
        t         (generalize (:ctx env') t1)
        [s2 t2]   (infer (assoc-in env' [:ctx x] t) e2)]
    [(sub/compose s1 s2) t2]))

(defmethod infer If [env expr]
  (let [[cond then else] (t/destructure expr)
        [s1 t1] (infer env cond)
        [s2 t2] (infer env then)
        [s3 t3] (infer env else)
        s4      (unify t1 (t/Sum 'Bool []))
        s5      (unify t2 t3)]
    [(sub/compose s5 s4 s3 s2 s1) (sub t2 s5)]))

(defmethod infer Seq [env expr]
  (let [expr' (map (comp second (partial infer env)) (.-a expr))
        s1    (unify-seq expr')
        ;; TODO: infer sequences^^?
        ]
    [s1 (t/Sum 'Expr [])]))

(defmethod infer Quo [env expr]
  (let [quoted? (:quoted? env)
        env'    (assoc env :quoted? true)]
    (infer env' (.-a expr))))
