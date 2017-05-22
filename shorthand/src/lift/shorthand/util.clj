(ns lift.shorthand.util
  (:require [clojure.string :as string]))

(defn res [s]
  (let [v (resolve s)
        ns (some-> v .ns .name str)
        name (some-> v .sym str)]
    (when (and ns name)
      (symbol ns name))))

(def word #"-|_|(?=[A-Z]+)")

(defn kebab-case [s]
  (->> (string/split s word)
       (map string/lower-case)
       (string/join "-")))

(defn predicate-name [x]
  (letfn [(predicate-ize [s]
            (->> (kebab-case s)
                 (format "type-%s?")))]
    (cond (symbol? x)  (symbol (predicate-ize (name x)))
          (keyword? x) (keyword (predicate-ize (name x)))
          (string? x)  (predicate-ize x)
          :else        (throw (ex-info (str "Don't know how to kebab:" x)
                                       {:type :unknown-type :x x})))))

(defn idx->key [i]
  (keyword (str (char (+ i 97)))))

(defn resolve-class [x]
  (let [v ((ns-map *ns*) x)]
    (cond (var? v)   (symbol (str (.getName (:ns (meta v))) \. x))
          (class? v) (symbol (.getName v))
          :else      x)))

(defn ns-qualify
  "Qualify symbol s by resolving it or using the current *ns*."
  [s]
  (if-let [ns-sym (some-> s namespace symbol)]
    (or (some-> (get (ns-aliases *ns*) ns-sym) str (symbol (name s))) s)
    (symbol (str (.name *ns*)) (str s))))


;; (defn type-var
;;   ;; not really correct - this assumes too much:
;;   ;; that the type var is in the first position of :vars
;;   ;; and that only d-specs have type vars
;;   [dspec]
;;   (when (d-spec-type? dspec)
;;     (when-let [v (first (.-vars dspec))]
;;       (when (var-type? v) v))))

;; (defn pair-ks->-type-vars [args return]
;;   (map (fn [k v] [{k (:op v)} {(type-var v) [k]}])
;;        (conj (map (fn [x] [:args (idx->key x)])
;;                   (range 0 (count args))) [:ret])
;;        (conj args return)))

;; (defn dependent-fn [[[k v]]]
;;   [k (get-in @type-env [`dependent (res v) :f])])

;; (defn parse-fn [args return]
;;   (let [tvars (keep type-var (conj args return))]
;;     (when (> (count tvars) 1)
;;       (let [fs-vs (pair-ks->-type-vars args return)
;;             fs    (->> fs-vs (map dependent-fn) (into {}))
;;             vs    (->> (map second fs-vs)
;;                        (apply merge-with concat {})
;;                        (#(select-keys % tvars))
;;                        (remove (fn [[_ v]] (< (count v) 2)))
;;                        (into {}))]
;;         `(fn [~'x]
;;            ~(->> vs
;;                  (map (fn [[k v]]
;;                         `(= ~@(map #(list (res (get fs %))
;;                                           (list `get-in 'x %))
;;                                    v))))
;;                  (cons 'and)))))))
  ;; ToSpec
  #_(to-spec [x]
    `(s/fspec :args ~(->> args
                          (map-indexed (fn [i v] [(idx->key i) (to-spec v)]))
                          (apply concat)
                          (cons `s/cat))
              ~@(when-let [f (parse-fn args return)] [:fn f])
              :ret ~(to-spec return)))

;; (basetype DSpec [op vars args]
;;   ISeq
;;   (seq [_] (apply list op args))
;;   Show
;;   (show [_]
;;     (let [sig (get-in @type-env [`dependent (res op) :sig])
;;           dep-type (first (type-seq sig))]
;;       (format "(%s: %s ** %s %s)"
;;               (string/join ", " (map pr-str vars))
;;               (pr-str dep-type)
;;               (pr-str op)
;;               (string/join " " (map pr-str args)))))
;;   ToSpec
;;   (to-spec [x] `(~op ~@(map to-spec args))))
