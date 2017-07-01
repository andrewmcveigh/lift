(ns lift.type.core
  (:refer-clojure :exclude [defn])
  (:require
   [lift.type.check :as check]
   [lift.type.syntax :as syn]
   [lift.type.type :as t]
   [lift.type.util :as u]))

(alias 'c 'clojure.core)

(defmacro tdef [sym & sig]
  `(let [sig# (t/parse-type-signature '~sig)]
     (swap! t/expr-env assoc
            '~sym (check/->Scheme (check/free sig#) sig#))))

(defmacro data [& decl]
  (t/data-cons (t/parse-data decl)))

(defmacro defn [name args expr]
  (let [[_ inferred] (check/infer
                      (check/map->Env @t/expr-env)
                      (syn/parse (list 'fn args expr)))
        declared     (:t (get @t/expr-env name))]
    (assert (check/unify inferred declared))
    `(c/defn ~name ~args ~expr)))
