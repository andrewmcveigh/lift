(ns lift.type.type
  (:refer-clojure :exclude [destructure read type])
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
  {:style/indent :defn}
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
              lift.type.type.Base
              lift.type.type.Show
              lift.type.type.Destructure]
            (when (seq fields)
              '[clojure.lang.ILookup])
            (map u/resolve-class (keys (dissoc ex 'Base 'Show)))))

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
                 (format "%s %s" ~(pr-str tagname) (apply pr-str ~fields)))])

         (destructure [_#] ~fields)

         ~@(apply concat (vals (dissoc ex 'Show))))

       (defn ~tagname ~fields (new ~classname ~@fields; 0 0
                                   ))
       (defn ~pred [~'x]
         (instance? ~classname ~'x))

       (defmethod print-method ~classname [~'x ~'writer]
         (.write ~'writer (show ~'x)))

       ~classname)))

(defn base? [x] (instance? Base x))

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
    (format "%s %s" (pr-str tag) (string/join " " (map pr-str vars)))))

(base Sum [tag vars]
  Show
  (show [_]
    (format "%s %s" (pr-str tag) (string/join " " (map pr-str vars)))))

(defn curry-syntax [op [a b & tail]]
  (if (seq tail)
    (op a (curry-syntax op (cons b tail)))
    (op a b)))

(defrecord Scheme [vars t])

;;; How much of this belongs in here?

(def type-env (atom {}))
(def expr-env (atom {}))

;;; Type language parsers

(s/def ::Unit #{()})

(s/def ::Const (s/and simple-symbol? #(re-matches #"^[A-Z][A-z]+$" (name %))))

(s/def ::Var (s/and simple-symbol? #(re-matches #"^[a-z\-]+$" (name %))))

(s/def ::Arrow
  (s/and seq?
         (s/cat ::type ::retype :more (s/+ (s/cat :_ #{'->} ::type ::retype)))))

(s/def ::Parameterized
  (s/alt :parens   (s/and seq? (s/cat ::Const ::Const :args (s/+ ::type)))
         :noparens (s/cat ::Const ::Const :args (s/+ ::type))))

;;; Type language constructors

(defmulti construct first)

(defmethod construct ::Unit  [[_ ast]] (Unit))

(defmethod construct ::Const [[_ ast]] (Const ast))
;;; TODO: type-name ^^ is not always a const type, e.g., in the case of Bool
;;; it's a Sum type, in the case of Point, it's a product. Theres a difference
;;; between the syntactic type-type, and the meaning, depends on env lookup

(defmethod construct ::Var   [[_ ast]] (Var ast))

(defmethod construct ::Arrow [[_ ast]]
  (->> (map ::type (:more ast))
       (cons (::type ast))
       (map construct)
       (curry-syntax Arrow)))

(defmethod construct ::Parameterized [[_ [_ x]]]
  (let [tag (::Const x)]
    (or (when-let [reg (get @type-env tag)]
          (when-let [ctor (cond (type-product? reg)
                                (partial Product tag)
                                (type-sum? reg)
                                (partial Sum tag))]
            (let [args (map construct (:args x))]
              (ctor args))))
        (throw (Exception. (format "Unknown Type %s" tag))))))

(s/def ::type
  (s/or ::Unit          ::Unit
        ::Const         ::Const
        ::Var           ::Var
        ::Arrow         ::Arrow
        ::Parameterized ::Parameterized))

(s/def ::retype
  (s/alt ::Unit          ::Unit
         ::Const         ::Const
         ::Var           ::Var
         ::Arrow         ::Arrow
         ::Parameterized ::Parameterized))

(defn parse [spec x]
  (let [conformed (s/conform spec x)]
    (if (s/invalid? conformed)
      (do
        (s/explain spec x)
        (throw (Exception. (format "Could not parse type signature %s" x))))
      (construct (s/conform spec x)))))

(defn parse-type-signature [sig]
  (if (= (count sig) 1)
    (parse ::retype sig)
    (parse ::type sig)))

(s/def ::product
  (s/cat ::type-cons ::type-cons := #{'=} :value-cons ::Parameterized))

(s/def ::parameterized-type-cons
  (s/cat ::Const ::Const ::type-params (s/+ ::Var)))

(s/def ::type-cons
  (s/alt :l-type-cons ::Const
         :p-type-cons ::parameterized-type-cons))

(s/def ::sum-cons
  (s/cat :type ::retype
         :more (s/+ (s/cat :| #{'|} :type ::retype))))

(s/def ::sum
  (s/cat ::type-cons ::type-cons := #{'=} ::sum-cons ::sum-cons))

(s/def ::data
  (s/or ::product ::product ::sum ::sum))

(defn parse-data [decl]
  (let [ast (s/conform ::data decl)]
    (if (s/invalid? ast)
      (do
        (s/explain ::expr decl)
        (throw (Exception. "Invalid Syntax")))
      ast)))

(defn type-cons [type node]
  (let [[t n] (::type-cons node)]
    (case t
      :l-type-cons `(~type '~n [])
      :p-type-cons `(~type '~(::Const n) (mapv Var '~(::type-params n))))))

(defn def-type-cons [type-cons]
  (swap! type-env assoc (.tag type-cons) type-cons))

(defn def-value-cons [sym signature]
  (swap! expr-env assoc
         (u/ns-qualify sym)
         (->Scheme (free signature) signature)))

(defn param-value-cons [[_ n] type-cons]
  (let [tag (::Const n)
        arglist (vec (take (count (:args n)) u/vars))]
    `((def-value-cons '~tag
        (curry-syntax Arrow (concat (map construct '~(:args n)) [~type-cons])))
      (base ~tag ~arglist))))

(defn value-cons [type [t n]]
  (case t
    ::Const `((def-value-cons '~n ~type)
              (def ~n
                (reify Show
                  (show [_#] ~(name n)))))
    ::Parameterized (param-value-cons n type)))

(defn sum-value-cons [node type-cons]
  (let [{:keys [type more]} (::sum-cons node)
        type-constructors (cons type (map :type more))]
    (mapcat (partial value-cons type-cons) type-constructors)))

(defn data-cons [[t n]]
  (case t
    ::product
    (let [type-cons (type-cons `Product n)]
      (concat
       `(do (def-type-cons ~type-cons))
       (param-value-cons (:value-cons n) type-cons)
       [type-cons]))
    ::sum
    (let [type-cons (type-cons `Sum n)]
      (concat
       `(do (def-type-cons ~type-cons))
       (sum-value-cons n type-cons)
       [type-cons]))))
