(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'io.github.typmk/parity)
(def version
  (or (System/getenv "RELEASE_VERSION")
      (format "0.1.%s-SNAPSHOT" (b/git-count-revs nil))))
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (let [basis @basis]
    (b/write-pom {:class-dir class-dir
                  :lib lib
                  :version version
                  :basis basis
                  :src-dirs ["src"]
                  :scm {:url "https://github.com/typmk/clojure.parity"
                        :connection "scm:git:https://github.com/typmk/clojure.parity.git"
                        :developerConnection "scm:git:https://github.com/typmk/clojure.parity.git"
                        :tag (str "v" version)}
                  :pom-data [[:description "Parity testing for Clojure compilers and runtimes"]
                             [:url "https://github.com/typmk/clojure.parity"]
                             [:licenses
                              [:license
                               [:name "Eclipse Public License 2.0"]
                               [:url "https://www.eclipse.org/legal/epl-2.0/"]]]]})
    (b/copy-dir {:src-dirs ["src"]
                 :target-dir class-dir})
    (b/jar {:class-dir class-dir
            :jar-file jar-file})
    (println (format "Built %s" jar-file))))

(defn deploy [_]
  (jar nil)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
   {:installer :remote
    :artifact (b/resolve-path jar-file)
    :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))
