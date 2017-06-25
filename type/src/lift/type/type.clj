(ns lift.type.type
  (:refer-clojure :exclude [type])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.type.util :as u]))

(alias 'c 'clojure.core)

(defprotocol Type)
(defprotocol Read (read [_]))
(defprotocol Show (show [_]))

(defmacro print-show [& types]
  `(do
     ~@(map (fn [t]
              `(defmethod print-method ~t [x# w#]
                 (.write w# (show x#))))
            types)))

(defmacro type
  {:style/indent [2 1]}
  [tagname & [fields? & extends]]
  (let [fields    (if (vector? fields?) fields? [])
        extends   (cond (vector? fields?) extends
                        fields? (cons fields? extends)
                        :else extends)
        classname (symbol (str (namespace-munge *ns*) ".types." tagname))
        ex        (if (seq extends)
                    (->> extends
                         (reduce (fn [[head & more :as init] x]
                                   (if (s/valid? (s/and simple-symbol?
                                                        #(re-matches #"^[A-Z]\w+$" (name %))) x)
                                     (conj init [x []])
                                     (conj more (update head 1 conj  x))))
                                 ())
                         (into {}))
                    {})
        pred      (u/predicate-name tagname)]
    `(do
       (declare ~pred)
       (deftype* ~(symbol (name (ns-name *ns*)) (name tagname))
         ~classname
         ~fields

         :implements
         ~(vec
           (concat
            '[clojure.lang.IType
              clojure.lang.IHashEq
              lift.t.type.Type
              lift.t.type.Show]
            (when (seq fields)
              '[clojure.lang.ILookup])
            (map u/resolve-class (keys (dissoc ex 'Type 'Show)))))

         (equals
          [_# ~'other]
          (and (instance? ~tagname ~'other)
               ~@(map (fn [n] `(= (. ~'other ~(symbol (str \- n))) ~n))
                      fields)))
         (hasheq
          [_#]
          (int (bit-xor ~(hash tagname)
                        (clojure.lang.APersistentMap/mapHasheq
                         ~(zipmap (map #(list 'quote %) fields) fields)))))
         (hashCode
          [_#]
          (clojure.lang.APersistentMap/mapHasheq
           ~(zipmap (map #(list 'quote %) fields) fields)))

         ~@(when (seq fields)
             `[(valAt
                [_# k#]
                (case k#
                  ~@(apply concat (map (fn [j] [(keyword j) j]) fields))
                  nil))
               (valAt
                [_# k# default#]
                (case k#
                  ~@(apply concat (map (fn [j] [(keyword j) j]) fields))
                  default#))])

         ~@(or (get ex 'Show)
              `[(lift.shorthand/show
                 [_]
                 (format "(%s %s)" ~(name tagname)
                         (apply pr-str ~fields)))])

         ~@(apply concat (vals (dissoc ex 'Show))))

       (defn ~tagname ~fields (new ~classname ~@fields; 0 0
                                   ))
       (defn ~pred [~'x]
         (instance? ~classname ~'x))

       (defmethod print-method ~classname [~'x ~'writer]
         (.write ~'writer (show ~'x)))

       ~classname)))

(defn type? [x] (instance? Type x))

(type Unit)
(type Const   [t])
(type Var     [v])
(type Arrow   [a b])
(type Product [a b])
(type Sum     [a b])
