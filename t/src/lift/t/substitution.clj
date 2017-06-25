(ns lift.t.substitution
  (:require
   [lift.f.functor :as f :refer [Functor]]
   [lift.t.type :as t :refer [Show]]
   [clojure.string :as string]))

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
