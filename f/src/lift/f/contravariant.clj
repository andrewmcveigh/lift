(ns lift.f.contravariant
  (:import [clojure.lang IFn]))

(defprotocol Contravariant
  (-contramap [x f]))

(extend-protocol Contravariant
  IFn
  (-contramap [x f]
    (comp x f)))

(defn contramap [f x]
  (-contramap x f))
