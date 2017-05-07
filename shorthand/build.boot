(def +project+ 'lift/shorthand)
(def +version+ "0.1.0-SNAPSHOT")
(def +description+ "Spec signature shorthand")

(def dependencies
  '[[org.clojure/clojure "1.9.0-alpha16"]
    [org.clojure/test.check "0.9.0"]
    [orchestra "0.3.0"]])

(set-env! :dependencies   dependencies
          :source-paths   #{"src"})

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
