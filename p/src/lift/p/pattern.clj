(ns lift.p.pattern
  (:refer-clojure :exclude [case destructure let])
  (:require
   [clojure.spec.alpha :as s]))

(alias 'c 'clojure.core)

(s/def ::type-name
  (s/and simple-symbol? #(re-matches #"^[A-Z][A-Za-z]*$" (name %))))

(s/def ::binding
  (s/or :type    ::type-name
        :literal (s/and #(not (s/valid? ::destructuring %))
                        (complement #{:as})
                        (complement symbol?))
        :binding symbol?))

(s/def ::destructuring
  (s/and vector?
         (s/cat :type (s/? ::type-name)
                :args (s/+ ::pattern)
                :bind (s/? (s/cat :as #{:as} :binding symbol?)))))

(s/def ::pattern
  (s/or :binding ::binding
        :destructuring ::destructuring))

(defprotocol Destructurable (-destructure [x]))

(defn arg-bindings [[t v g?]]
  (c/case t
    :binding (c/let [[btype binding] v]
               (c/case btype
                 :type '_
                 :literal '_
                 :binding binding))
    :destructuring g?))

(defn literal= [destructured args]
  (->> destructured
       (map (fn [[_ [btype a]] b] (when (= :literal btype) (= a b))) args)
       (remove nil?)
       (every? true?)))

(defn ensure-class [class-or-symbol]
  (if (class? class-or-symbol)
    class-or-symbol
    (resolve class-or-symbol)))

(defn let-destructure [match mval value expr]
  (c/case match
    :destructuring
    (c/let [args (map (fn [[t :as v]]
                        (c/case t
                          :binding v
                          :destructuring (conj v (gensym))))
                      (:args mval))
            bind (:bind mval)
            type (:type mval)
            dest (remove (comp #{:binding} first) args)
            dsym (gensym)]
      `(let [type?# (and ~type (instance? (ensure-class ~type) ~value))]
         (when (or type?# (nil? ~type))
           (c/let [value# ~value
                   ~dsym (if type?# (-destructure value#) value#)
                   ~(if bind (:binding bind) (gensym)) value#
                   [~@(map arg-bindings args)] ~dsym]
             (or ~(when (some (comp #{:literal} first second) args)
                    `(when (literal= ~dsym '~args)
                       ~(if (seq dest)
                          (apply let-destructure (conj (first dest) expr))
                          expr)))
                 ~(when (every? (comp #{:binding} first second) args) expr)
                 ~(when (not (some (comp #{:literal} first second) args))
                    (if (seq dest)
                      (apply let-destructure (conj (first dest) expr))
                      expr)))))))
    :binding
    (c/let [[match' value'] mval]
      (c/case match'
        :type `(when (instance? ~(ensure-class value') ~value) ~expr)
        :literal `(when (= ~value' ~value) ~expr)
        :binding `(c/let [~value' ~value] ~expr)))))

(defmacro let [bindings expr]
  (c/let [[bind value] (take 2 bindings)]
    (when (and bind value)
      (c/let [[match mval] (s/conform ::pattern bind)]
        (let-destructure match mval value expr)))))

(defmacro case
  {:style/indent :defn}
  [e & bindings]
  (cons 'or
        (map (fn [[pattern expr]] `(let [~pattern ~e] ~expr))
             (partition 2 bindings))))
