(ns lift.type.core
  (:refer-clojure :exclude [defn])
  (:require
   [lift.type.check :as check]
   [lift.type.syntax :as syn]
   [lift.type.type :as t]
   [lift.type.util :as u]))

(alias 'c 'clojure.core)

(defmacro tdef [sym & sig]
  `(let [sym# (u/resolve-sym '~sym)
         sig# (t/parse-type-signature '~sig)]
     (swap! t/expr-env assoc
            sym# (t/->Scheme (t/free sig#) sig#))))

(defmacro data
  {:style/indent :defn}
  [& decl]
  (t/data-cons (t/parse-data decl)))

(defmacro check [expr]
  `(->> '~expr
        (syn/parse)
        (check/infer (check/map->Env @t/expr-env))
        (second)))

(defmacro defn [name args expr]
  (let [[_ inferred] (check/infer
                      (check/map->Env @t/expr-env)
                      (syn/parse (list 'fn args expr)))
        declared     (:t (get @t/expr-env (u/resolve-sym name)))]
    (assert (check/unify inferred declared))
    `(c/defn ~name ~args ~expr)))
