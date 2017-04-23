(ns lift.f.functor-test
  (:require [lift.f.functor :as sut]
            [clojure.test :as t]))

(defmacro race [f1 f2]
  `(let [start# (. System (nanoTime))
         ret1# (~f1)
         end1# (. System (nanoTime))
         run1# (- end1# start#)
         ret2# (~f2)
         end2# (. System (nanoTime))
         run2# (- end2# end1#)
         ret3# (~f2)
         end3# (. System (nanoTime))
         run3# (- end3# end2#)
         ret4# (~f1)
         end4# (. System (nanoTime))
         run4# (- end4# end3#)
         ret5# (~f1)
         end5# (. System (nanoTime))
         run5# (- end5# end4#)
         ret6# (~f2)
         end6# (. System (nanoTime))
         run6# (- end6# end5#)
         ret7# (~f2)
         end7# (. System (nanoTime))
         run7# (- end7# end6#)
         ret8# (~f1)
         end8# (. System (nanoTime))
         run8# (- end8# end7#)
         f1# (float (/ (+ run1# run4# run5# run8#) 4000000))
         f2# (float (/ (+ run2# run3# run6# run7#) 4000000))]
     (println "Run:")
     (printf "f1: %sms\nf2: %sms\n" f1# f2#)
     (prn)))

(set! *warn-on-reflection* true)

(defn dorace []
  (let [x (into {} (c/map first (s/exercise (s/tuple keyword? int?) 10000)))
        ^PersistentHashMap x x
        f identity
        ^IFn f f
        f1 (fn []
             (.persistent
              ^PersistentHashMap$TransientHashMap
              (.kvreduce x
                         (fn [^PersistentHashMap$TransientHashMap i k v]
                           (.assoc i k (.invoke f v)))
                         (.asTransient x))))

        f2 (fn []
             (persistent!
              (.kvreduce x
                         (fn [^PersistentHashMap$TransientHashMap i k v]
                           (.assoc i k (.invoke f v)))
                         (.asTransient x))))

        f3 (fn []
             (.kvreduce x
                        (fn [^clojure.lang.PersistentHashMap i k v]
                          (.assoc i k (.invoke f v)))
                        x))

                      ]
                  (race f3 f2)))
