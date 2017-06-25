(ns lift.type.util
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
