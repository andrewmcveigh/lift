(ns lift.f.bifunctor
  (:refer-clojure :exclude [first second])
  (:import [clojure.lang IFn PersistentArrayMap PersistentHashMap]))

(alias 'c 'clojure.core)

(defprotocol Bifunctor
  (-bimap  [p f g])
  (-first  [p f])
  (-second [p f]))

(extend-protocol Bifunctor
  PersistentHashMap
  (-bimap [^PersistentHashMap p ^IFn f ^IFn g]
    (.kvreduce p
               (fn [^PersistentHashMap i k v]
                 (.assoc i (.invoke f k) (.invoke g v)))
               p))
  (-first [^PersistentHashMap p ^IFn f]
    (.kvreduce p
               (fn [^PersistentHashMap i k v]
                 (.assoc i (.invoke f k) v))
               p))
  (-second [^PersistentHashMap p ^IFn g]
    (.kvreduce p
               (fn [^PersistentHashMap i k v]
                 (.assoc i k (.invoke g v)))
               p))

  PersistentArrayMap
  (-bimap [^PersistentHashMap p ^IFn f ^IFn g]
    (.kvreduce p
               (fn [^PersistentHashMap i k v]
                 (.assoc i (.invoke f k) (.invoke g v)))
               p))
  (-first [^PersistentHashMap p ^IFn f]
    (.kvreduce p
               (fn [^PersistentHashMap i k v]
                 (.assoc i (.invoke f k) v))
               p))
  (-second [^PersistentHashMap p ^IFn g]
    (.kvreduce p
               (fn [^PersistentHashMap i k v]
                 (.assoc i k (.invoke g v)))
               p)))

(defn bimap [f g p]
  (-bimap p f g))

(defn first [f p]
  (-first p f))

(defn second [g p]
  (-second p g))
