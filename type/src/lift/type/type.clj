(ns lift.type.type
  (:refer-clojure :exclude [destructure instance? read type])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [lift.type.util :as u]))

(alias 'c 'clojure.core)

(defprotocol Base)
(defprotocol Read        (read [_]))
(defprotocol Show        (show [_]))
(defprotocol Destructure (destructure [_]))
(defprotocol Free        (free [_]))

(defmacro print-show [& types]
  `(do
     ~@(map (fn [t]
              `(defmethod print-method ~t [x# w#]
                 (.write w# (show x#))))
            types)))

(defmethod print-method Object [x w]
  (if (satisfies? Show x)
    (.write w (show x))
    (.write w (#'clojure.core/print-object x w))))

(defn base-classname [tagname]
  (symbol (str (namespace-munge *ns*) ".types." tagname)))

(defmacro base
  {:style/indent [2 1]}
  [tagname & [fields? & extends]]
  (let [fields    (if (vector? fields?) fields? [])
        extends   (cond (vector? fields?) extends
                        fields? (cons fields? extends)
                        :else extends)
        classname (base-classname tagname)
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
        pred      (u/predicate-name tagname)
        gs        (gensym)]
    `(do
       (declare ~pred)
       (deftype* ~(symbol (name (ns-name *ns*)) (name tagname))
         ~classname
         ~(conj fields '___meta)

         :implements
         ~(vec
           (concat
            '[clojure.lang.IType
              clojure.lang.IHashEq
              clojure.lang.IObj
              lift.type.type.Base
              lift.type.type.Show
              lift.type.type.Destructure]
            (when (seq fields)
              '[clojure.lang.ILookup])
            (map u/resolve-class (keys (dissoc ex 'Base 'Show)))))

         (equals
          [_# ~'other]
          (and (c/instance? ~tagname ~'other)
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

         (meta [_#] ~'___meta)
         (withMeta [_# ~gs] (new ~tagname ~@fields ~gs))
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
                 (format "%s %s" ~(pr-str tagname) (apply pr-str ~fields)))])

         (destructure [_#] ~fields)

         ~@(apply concat (vals (dissoc ex 'Show))))

       (defn ~tagname ~fields (new ~classname ~@fields nil; 0 0
                                   ))
       (defn ~pred [~'x]
         (c/instance? ~classname ~'x))

       (defmethod print-method ~classname [~'x ~'writer]
         (.write ~'writer (show ~'x)))

       ~classname)))

(defn base? [x] (satisfies? Base x))

(base Unit
  Show
  (show [_] "()"))

(base Const [t]
  Show
  (show [_] (pr-str t)))

(base Var [v]
  Show
  (show [_] (pr-str v)))

(base Arrow [a b]
  Show
  (show [_]
    (format "(%s -> %s)" (pr-str a) (pr-str b))))

(base Product [tag vars]
  Show
  (show [_]
    (if (seq vars)
      (format "%s %s" (pr-str tag) (string/join " " (map pr-str vars)))
      (pr-str tag))))

(base Sum [tag vars]
  Show
  (show [_]
    (if (seq vars)
      (format "%s %s" (pr-str tag) (string/join " " (map pr-str vars)))
      (pr-str tag))))

(base Constraint [tag vars]
  Show
  (show [_]
    (if (seq vars)
      (format "%s %s" (pr-str tag) (string/join " " (map pr-str vars)))
      (pr-str tag))))

(base Constrained [constraint type]
  Show
  (show [_]
    (format "%s => %s" (pr-str constraint) (pr-str type))))

(defn curry-syntax [op [a b & tail]]
  (if (seq tail)
    (op a (curry-syntax op (cons b tail)))
    (op a b)))

(defrecord Scheme [vars t])

;;; How much of this belongs in here?

(def type-env (atom {}))
(def expr-env (atom {}))

(defn arglist [t]
  (if-let [b (and (type-arrow? t) (.b t))]
    (cons (.a t) (arglist b))
    ()))

(defn instance? [instance]
  (-> @type-env
      (get-in [(.tag instance) :instances])
      (contains? instance)))
