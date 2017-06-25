(ns lift.shorthand.syntax-quote
  (:import
   (clojure.lang PersistentHashSet IMeta RT Symbol Reflector Var IObj
                 PersistentVector IRecord Namespace)
   (java.util regex.Pattern List LinkedList)))

(defn ^:private ns-name* [x]
  (if (instance? Namespace x)
    (name (ns-name x))
    (name x)))

(def ^:dynamic *alias-map*
  "Map from ns alias to ns, if non-nil, it will be used to resolve read-time
   ns aliases instead of (ns-aliases *ns*).
   Defaults to nil"
  nil)

(defn- resolve-ns [sym]
  (or ((or *alias-map*
           (ns-aliases *ns*)) sym)
      (find-ns sym)))

(def ^:private ^:dynamic gensym-env nil)

(declare syntax-quote)

(defn- unquote-splicing? [form]
  (and (seq? form)
       (= (first form) 'clojure.core/unquote-splicing)))

(defn- unquote? [form]
  (and (seq? form)
       (= (first form) 'clojure.core/unquote)))

(defn- expand-list
  "Expand a list by resolving its syntax quotes and unquotes"
  [s]
  (loop [s (seq s) r (transient [])]
    (if s
      (let [item (first s)
            ret (conj! r
                       (cond
                        (unquote? item)          (list 'clojure.core/list (second item))
                        (unquote-splicing? item) (second item)
                        :else                    (list 'clojure.core/list (syntax-quote item))))]
        (recur (next s) ret))
      (seq (persistent! r)))))

(defn- flatten-map
  "Flatten a map into a seq of alternate keys and values"
  [form]
  (loop [s (seq form) key-vals (transient [])]
    (if s
      (let [e (first s)]
        (recur (next s) (-> key-vals
                          (conj! (key e))
                          (conj! (val e)))))
      (seq (persistent! key-vals)))))

(defn- register-gensym [sym]
  (if-not gensym-env
    (throw (IllegalStateException. "Gensym literal not in syntax-quote")))
  (or (get gensym-env sym)
      (let [gs (symbol (str (subs (name sym)
                                  0 (dec (count (name sym))))
                            "__" (RT/nextID) "__auto__"))]
        (set! gensym-env (assoc gensym-env sym gs))
        gs)))

(defn ^:dynamic resolve-symbol
  "Resolve a symbol s into its fully qualified namespace version"
  [s]
  (if (pos? (.indexOf (name s) "."))
    s ;; If there is a period, it is interop
    (if-let [ns-str (namespace s)]
      (let [ns (resolve-ns (symbol ns-str))]
        (if (or (nil? ns)
                (= (ns-name* ns) ns-str)) ;; not an alias
          s
          (symbol (ns-name* ns) (name s))))
      (if-let [o ((ns-map *ns*) s)]
        (if (class? o)
          (symbol (.getName ^Class o))
          (if (var? o)
            (symbol (-> ^Var o .ns ns-name*)
                    (-> ^Var o .sym name))))
        (symbol (ns-name* *ns*) (name s))))))

(defn- add-meta [form ret]
  (if (and (instance? IObj form)
           (seq (dissoc (meta form) :line :column :end-line :end-column :file :source)))
    (list 'clojure.core/with-meta ret (syntax-quote (meta form)))
    ret))

(defn- syntax-quote-coll [type coll]
  ;; We use sequence rather than seq here to fix http://dev.clojure.org/jira/browse/CLJ-1444
  ;; But because of http://dev.clojure.org/jira/browse/CLJ-1586 we still need to call seq on the form
  (let [res (list 'clojure.core/sequence
                  (list 'clojure.core/seq
                        (cons 'clojure.core/concat
                              (expand-list coll))))]
    (if type
      (list 'clojure.core/apply type res)
      res)))

(defn map-func
  "Decide which map type to use, array-map if less than 16 elements"
  [coll]
  (if (>= (count coll) 16)
    'clojure.core/hash-map
    'clojure.core/array-map))

(defn syntax-quote [form]
  (->>
   (cond
    (special-symbol? form) (list 'quote form)

    (symbol? form)
    (list 'quote
          (if (namespace form)
            (let [maybe-class ((ns-map *ns*)
                               (symbol (namespace form)))]
              (if (class? maybe-class)
                (symbol (.getName ^Class maybe-class) (name form))
                (resolve-symbol form)))
            (let [sym (name form)]
              (cond
               (.endsWith sym "#")
               (register-gensym form)

               (.startsWith sym ".")
               form

               (.endsWith sym ".")
               (let [csym (symbol (subs sym 0 (dec (count sym))))]
                 (symbol (.concat (name (resolve-symbol csym)) ".")))
               :else (resolve-symbol form)))))

    (unquote? form) (second form)
    (unquote-splicing? form) (throw (IllegalStateException. "unquote-splice not in list"))

    (coll? form)
    (cond

     (instance? IRecord form) form
     (map? form) (syntax-quote-coll (map-func form) (flatten-map form))
     (vector? form) (list 'clojure.core/vec (syntax-quote-coll nil form))
     (set? form) (syntax-quote-coll 'clojure.core/hash-set form)
     (or (seq? form) (list? form))
     (let [seq (seq form)]
       (if seq
         (syntax-quote-coll nil seq)
         '(clojure.core/list)))

     :else (throw (UnsupportedOperationException. "Unknown Collection type")))

    (or (keyword? form)
        (number? form)
        (char? form)
        (string? form)
        (nil? form)
        (instance? Boolean form)
        (instance? Pattern form))
    form

    :else (list 'quote form))
   (add-meta form)))
