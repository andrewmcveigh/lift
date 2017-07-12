(ns lift.lang)

(defn lang [ns]
  (clojure.core/ns ns
    (:require [lift.type.prelude :refer :all]))
  `~lang)

;; (clojure.java.io/resource "data_readers.clj")

(set! *data-readers* (assoc *data-readers* 'lift #'lift.lang/lang))
