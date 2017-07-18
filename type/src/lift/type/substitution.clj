(ns lift.type.substitution
  (:require
   [lift.f.functor :as f :refer [Functor]]
   [lift.type.type :as t :refer [Show]]
   [clojure.string :as string])
  (:import
   [lift.type.type Env Scheme]
   [lift.type.type.types
    Unit Const Var Arrow Product Sum Constraint Constrained]))

(defprotocol Substitutable
  (sub [_ subst]))

(deftype Sub [value]
  clojure.lang.IHashEq
  (equals [_ other]
    (and (instance? Sub other)
         (= value (.value other))))
  (equiv [_ other]
    (and (instance? Sub other)
         (= value (.value other))))
  clojure.lang.ILookup
  (valAt [x k]
    (.valAt x k nil))
  (valAt [_ k not-found]
    (or (some (fn [[t v]] (when (= k v) t)) value) not-found))
  clojure.lang.IPersistentMap
  (without [_ key]
    (Sub. (remove (comp #{key} first) value)))
  Functor
  (-map [_ f]
    (Sub. (f/map f value)))
  Show
  (show [_]
    (str "#sub ["
         (->> value
              (keep (fn [[t v :as subst]]
                      (when (seq subst)
                        (str (pr-str t) " / " (pr-str v)))))
              (string/join ", ")) "]")))

(t/print-show Sub)

(def id (Sub. [[]]))

(defn singleton [var term]
  (Sub. [[term var]]))

(defn compose [& substitutions]
  (reduce (fn [s t]
            (if (instance? Sub t)
              (Sub. (into (.value (sub t s)) (.value s)))
              (throw (Exception. "Argument `t` must be a substitution"))))
          (reverse substitutions)))

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
