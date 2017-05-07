(def +project+ 'lift/lift)
(def +version+ "0.1.0-SNAPSHOT")
(def +description+ "Abstract nonsense for Clojure")

(def dependencies
  `[[org.clojure/clojure "1.9.0-alpha16"]
    [lift/f ~+version+]
    [lift/shorthand ~+version+]])

(def test-dependencies
  '[[org.clojure/test.check "0.9.0"]])

(require '[clojure.java.io :as io])

(def src-dirs
  (->> (io/file ".")
       (.listFiles)
       (filter (fn [f]
                 (and (.isDirectory f)
                      (contains? (set (map #(.getName %)
                                           (.listFiles f)))
                                 "src"))))
       (map #(str (.getName %) "/src"))
       (set)))

(set-env! :dependencies (vec (concat dependencies test-dependencies))
          :source-paths src-dirs)

(task-options!
 pom {:project +project+
      :version +version+
      :description +description+
      :license {"Eclipse Public License"
                "http://www.eclipse.org/legal/epl-v10.html"}
      :scm {:url "git@github.com:andrewmcveigh/lift.git"}}
 push {:repo "clojars"}
 target {:dir #{"target"}})

(defn cider? []
  (get (ns-publics 'boot.user) 'cider))

(replace-task!
 [r repl] (comp ((or (cider?) (constantly identity))) r))

(deftask build []
  (comp (pom) (jar) (target)))

(deftask deploy []
  (comp (build) (install) (push)))
