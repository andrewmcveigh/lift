(ns lift.type.core
  (:refer-clojure :exclude [defn type])
  (:require
   [lift.type.check :as check]
   [lift.type.parse :as parse]
   [lift.type.syntax :as syn]
   [lift.type.type :as t]
   [lift.type.util :as u]))

(alias 'c 'clojure.core)

(defmacro tdef [sym & sig]
  `(let [sym# (u/resolve-sym '~sym)
         sig# (parse/type-signature '~sig)]
     (swap! t/expr-env assoc sym# (t/->Scheme (t/free sig#) sig#))
     sig#))

(defmacro data
  {:style/indent :defn}
  [& decl]
  (parse/data-cons (parse/data decl)))

(c/defn -check [expr]
  (->> expr
       (syn/parse)
       (check/infer (check/map->Env @t/expr-env))
       (second)))

(defmacro check [expr]
  `(-check '~expr))

(defmacro defn [name args expr]
  (let [inferred (-check (list 'fn args expr))
        declared     (:t (get @t/expr-env (u/resolve-sym name)))]
    (assert (check/unify inferred declared))
    `(c/defn ~name ~args ~expr)))

(defmacro T [type-name]
  (get @t/type-env type-name))

(defmacro const [name parser]
  `(let [t# (t/Const '~name)]
     (swap! t/type-env assoc '~name t#)
     (syn/deflit ~name ~parser)
     t#))

(c/defn type [x]
  (or (some-> x meta :type)
      (let [[t n] (syn/parse x)]
        (when (= ::syn/Lit t) (t/Const n)))))
