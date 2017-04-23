(ns lift.f.functor
  (:refer-clojure :exclude [map])
  (:import [clojure.lang IFn PersistentArrayMap PersistentHashMap
            PersistentHashSet PersistentVector PersistentList]))

(alias 'c 'clojure.core)

(defprotocol Functor
  (-map [x f]))

(extend-protocol Functor
  PersistentHashMap
  (-map [^PersistentHashMap x ^IFn f]
    (.kvreduce x
               (fn [^PersistentHashMap i k v]
                 (.assoc i k (.invoke f v)))
               x))

  PersistentArrayMap
  (-map [^PersistentArrayMap x ^IFn f]
    (.kvreduce x
               (fn [^PersistentArrayMap i k v]
                 (.assoc i k (.invoke f v)))
               x))

  PersistentHashSet
  (-map [x f]
    (into #{} (c/map f x)))

  PersistentList
  (-map [x f]
    (c/map f x))

  PersistentVector
  (-map [x f]
    (mapv f x))

  IFn
  (-map [x f]
    (comp f x)))

(defn map [f x]
  (-map x f))
