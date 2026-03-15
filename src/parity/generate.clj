#!/usr/bin/env clojure
;; =============================================================================
;; generate.clj — reflect on JVM, generate tests, capture reference, emit cljc
;; =============================================================================

(require '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[clojure.edn :as edn])

;; Load all namespaces for reflection
(def shipped-namespaces
  '[clojure.set clojure.walk clojure.data clojure.datafy clojure.edn
    clojure.math clojure.repl clojure.test clojure.zip clojure.xml
    clojure.stacktrace clojure.template clojure.instant clojure.reflect
    clojure.main clojure.java.browse clojure.java.javadoc clojure.java.process
    clojure.java.shell clojure.core.reducers clojure.core.protocols
    clojure.core.server clojure.spec.alpha clojure.spec.gen.alpha
    clojure.spec.test.alpha])

(def contrib-namespaces
  '[clojure.core.async clojure.core.cache clojure.core.memoize
    clojure.data.csv clojure.data.json clojure.data.xml clojure.data.zip
    clojure.data.priority-map clojure.data.int-map clojure.data.avl
    clojure.data.codec.base64 clojure.java.data clojure.java.classpath
    clojure.math.combinatorics clojure.math.numeric-tower
    clojure.test.check clojure.test.check.generators
    clojure.test.check.properties clojure.test.check.clojure-test
    clojure.tools.cli clojure.tools.logging clojure.tools.namespace.find
    clojure.tools.namespace.repl clojure.tools.reader clojure.tools.reader.edn
    clojure.tools.trace clojure.algo.generic.arithmetic
    clojure.algo.generic.collection clojure.algo.generic.functor
    clojure.algo.generic.math-functions clojure.algo.monads
    clojure.core.logic clojure.core.match clojure.core.unify])

;; Lazy — only load namespaces when needed (contrib is heavy)
(defn load-namespaces! [scope]
  (let [nss (case scope
              :lang shipped-namespaces
              :contrib contrib-namespaces
              :all (concat shipped-namespaces contrib-namespaces))]
    (doseq [ns-sym nss]
      (try (require ns-sym) (catch Exception _)))))

(def default-namespaces
  (vec (map str (cons 'clojure.core shipped-namespaces))))

(def all-namespaces
  (vec (concat default-namespaces (map str contrib-namespaces))))

;; =============================================================================
;; Type-driven test values — one good value per type, plus nil and empty
;; =============================================================================

(def values
  {:num     {:good "42"       :nil "nil"   :empty "0"}
   :string  {:good "\"hello\"" :nil "nil"  :empty "\"\""}
   :keyword {:good ":a"       :nil "nil"}
   :symbol  {:good "'foo"     :nil "nil"}
   :coll    {:good "[1 2 3]"  :nil "nil"   :empty "[]"}
   :vec     {:good "[1 2 3]"  :nil "nil"   :empty "[]"}
   :map     {:good "{:a 1}"   :nil "nil"   :empty "{}"}
   :set     {:good "#{1 2 3}" :nil "nil"   :empty "#{}"}
   :list    {:good "'(1 2 3)" :nil "nil"   :empty "'()"}
   :fn      {:good "inc"}
   :pred    {:good "even?"}
   :regex   {:good "#\"\\\\d+\""}
   :xf      {:good "(map inc)"}
   :char    {:good "\\a"}
   :any     {:good "42"       :nil "nil"   :empty "[]"}})

(defn arg-type
  "Infer value type from arg name and namespace context."
  [arg-name ns-name]
  (let [s (str arg-name)]
    (cond
      (= s "&")                                          nil
      (#{"n" "num" "x" "y" "a" "b" "start" "end"
         "step" "init" "from" "to" "index" "i" "j"} s)  :num
      (#{"s" "string" "cs" "substr" "replacement"} s)    :string
      (#{"coll" "c" "c1" "c2" "c3" "colls"} s)          :coll
      (#{"f" "fn" "pred" "g"} s)                         :fn
      (#{"xform" "xf"} s)                                :xf
      (#{"k" "key" "t" "tag"} s)                         :keyword
      (#{"v" "val" "e"} s)                               :any
      (#{"m" "map" "kmap" "smap"} s)                     :map
      (#{"re" "pattern" "match"} s)                      :regex
      (#{"xrel" "yrel" "xset" "s1" "s2"} s)             :set
      (#{"ch"} s)                                        :char
      (str/ends-with? s "map")                           :map
      (str/ends-with? s "set")                           :set
      (str/ends-with? s "?")                             :any
      (= ns-name "clojure.string")                       :string
      (= ns-name "clojure.math")                         :num
      (= ns-name "clojure.set")                          :set
      :else                                              :any)))

(def nilable-types
  "Types where nil/empty are meaningful inputs (not just error-producing)."
  #{:coll :vec :map :set :list :any :string})

(defn test-values-for
  "Generate test value sets for an arg: [{:label suffix :val string}]."
  [arg-name ns-name]
  (let [typ (arg-type arg-name ns-name)
        v (get values typ (:any values))]
    (cond-> [{:label (str arg-name) :val (:good v)}]
      (and (:nil v) (nilable-types typ))     (conj {:label (str arg-name "=nil") :val "nil"})
      (and (:empty v) (nilable-types typ))   (conj {:label (str arg-name "=empty") :val (:empty v)}))))

;; =============================================================================
;; Var classification
;; =============================================================================

(def skip-vars
  "Vars that can't be tested with simple expression eval."
  (into #{}
    (mapcat identity
      [;; Dynamic vars
       '[*ns* *out* *err* *in* *file* *command-line-args* *print-length*
         *print-level* *print-meta* *print-dup* *print-readably* *flush-on-newline*
         *read-eval* *data-readers* *default-data-reader-fn* *assert*
         *math-context* *agent* *1 *2 *3 *e *warn-on-reflection*
         *unchecked-math* *compiler-options* *compile-path* *compile-files*
         *allow-unresolved-vars* *reader-resolver* *source-path*
         *use-context-classloader* *verbose-defrecords* *fn-loader* *suppress-read*]
       ;; Side effects / destructive
       '[shutdown-agents alter-var-root intern ns-unmap ns-unalias remove-ns
         in-ns load load-file load-reader load-string require use import
         refer refer-clojure compile gen-class gen-interface]
       ;; Need context (agent, ref, atom, volatile, promise, dosync)
       '[send send-off send-via restart-agent await await-for await1
         add-watch remove-watch set-validator! dosync commute alter ref-set
         ensure swap! reset! swap-vals! reset-vals! vswap! vreset! deliver]
       ;; Async / IO
       '[future future-call pmap pcalls pvalues locking io!
         slurp spit print println pr prn printf newline flush
         read read-line read-string]
       ;; Test runners (side effects, run all loaded tests)
       '[run-tests run-all-tests run-test run-test-var test-var test-vars
         test-ns test-all-vars do-report inc-report-counter
         set-test with-test deftest deftest- testing is are try-expr
         assert-any assert-predicate use-fixtures]
       ;; Need hierarchy / eval context
       '[class type supers bases parents ancestors descendants
         make-hierarchy isa? derive underive
         eval macroexpand macroexpand-1 special-symbol? find-keyword]
       ;; Binding forms
       '[binding with-bindings with-bindings* with-redefs with-redefs-fn]])))

(defn classify-var [sym var-meta]
  (let [nm (name sym)
        arglists (:arglists var-meta)]
    (cond
      (skip-vars sym)                 :skip
      (nil? arglists)                 :skip
      (str/starts-with? nm "->")      :skip
      (str/starts-with? nm "map->")   :skip
      (> (apply min (map #(count (take-while (fn [a] (not= '& a)) %)) arglists)) 4) :skip
      :else                           :testable)))

;; =============================================================================
;; Test generation — intent-driven, not brute-force
;; =============================================================================

(defn arg-name-str [arg]
  (cond (symbol? arg) (name arg) (vector? arg) "v" (map? arg) "m" :else "x"))

(defn gen-tests
  "Generate tests for a single var. Returns seq of {:it :eval} maps."
  [sym var-meta ns-name]
  (let [arglists (:arglists var-meta)
        qualified (if (= ns-name "clojure.core") (name sym) (str ns-name "/" (name sym)))
        ;; Pick shortest non-zero arity
        sorted (sort-by count arglists)
        best-arity (or (first (filter #(pos? (count %)) sorted)) (first sorted))
        fixed-args (vec (take-while #(not= '& %) best-arity))
        arity (count fixed-args)
        has-nullary? (some #(zero? (count %)) arglists)
        is-pred? (and (str/ends-with? (name sym) "?") (= 1 arity))]
    (cond->
      []
      ;; Nullary
      has-nullary?
      (conj {:it (name sym) :eval (str "(" qualified ")")})

      ;; Predicate: test against each type
      is-pred?
      (into (for [v ["nil" "true" "42" "3.14" "\"hello\"" ":a" "[1 2]" "{:a 1}" "#{1}"]]
              {:it (str (name sym) " " v) :eval (str "(" qualified " " v ")")}))

      ;; Normal function: happy path + nil + empty per arg
      (and (not is-pred?) (pos? arity) (<= arity 4))
      (into
        (let [arg-names (mapv arg-name-str fixed-args)
              arg-test-sets (mapv #(test-values-for %1 ns-name) fixed-args)]
          ;; Happy path: first (good) value for each arg
          (let [happy-args (mapv #(:val (first %)) arg-test-sets)
                happy-label (str (name sym) " " (str/join " " happy-args))]
            (cond-> [{:it happy-label
                      :eval (str "(" qualified " " (str/join " " happy-args) ")")}]
              ;; Nil test: first arg = nil, rest = good
              (and (> arity 0) (:nil (get values (arg-type (first fixed-args) ns-name))))
              (conj (let [args (assoc happy-args 0 "nil")]
                      {:it (str (name sym) " nil " (str/join " " (rest args)))
                       :eval (str "(" qualified " " (str/join " " args) ")")}))
              ;; Empty test: first arg = empty, rest = good
              (and (> arity 0) (:empty (get values (arg-type (first fixed-args) ns-name))))
              (conj (let [empty-val (:empty (get values (arg-type (first fixed-args) ns-name)))
                          args (assoc happy-args 0 empty-val)]
                      {:it (str (name sym) " " empty-val " " (str/join " " (rest args)))
                       :eval (str "(" qualified " " (str/join " " args) ")")})))))))))

;; =============================================================================
;; Namespace processing
;; =============================================================================

(defn process-namespace [ns-name]
  (try
    (require (symbol ns-name))
    (let [publics (sort-by key (ns-publics (symbol ns-name)))
          tests (vec (mapcat (fn [[sym v]]
                               (when (= :testable (classify-var sym (meta v)))
                                 (gen-tests sym (meta v) ns-name)))
                             publics))]
      {:ns ns-name :total (count publics) :tests tests
       :generated (count tests) :skipped (- (count publics) (count (filter #(= :testable (classify-var (key %) (meta (val %)))) publics)))})
    (catch Exception e
      {:ns ns-name :total 0 :tests [] :generated 0 :skipped 0 :error (str e)})))

;; =============================================================================
;; Output — write spec .edn files
;; =============================================================================

(defn write-specs [results lang-dir contrib-dir]
  (let [contrib-set (set (map str contrib-namespaces))]
    (doseq [{:keys [ns tests]} results :when (seq tests)]
      (let [dir (if (contrib-set ns) contrib-dir lang-dir)
            path (str dir "/" ns ".edn")]
        (io/make-parents path)
        (spit path (pr-str [{:category (keyword ns) :tests tests}]))
        (println (format "  %-45s %d tests" path (count tests)))))))

(defn print-stats [results]
  (let [total-tests (reduce + (map :generated results))
        total-vars (reduce + (map :total results))]
    (println (format "\n  %d namespaces, %d vars, %d tests generated\n"
                     (count results) total-vars total-tests))
    (doseq [{:keys [ns total generated skipped error]} results]
      (if error
        (println (format "  %-45s ERROR: %s" ns error))
        (println (format "  %-45s %4d vars  %4d tests  %4d skipped"
                         ns total generated skipped))))))

;; =============================================================================
;; Expand — spec files -> flat expression list
;; =============================================================================

(def base-dir (or (System/getProperty "parity.dir") "."))
(def spec-dirs [(str base-dir "/lang") (str base-dir "/contrib")])
(def results-dir (str base-dir "/results"))
(def expressions-file (str results-dir "/expressions.edn"))
(def reference-file (str results-dir "/reference.edn"))

(defn spec-files []
  (->> spec-dirs
       (mapcat #(file-seq (io/file %)))
       (filter #(and (.isFile %) (str/ends-with? (.getName %) ".edn")))
       (sort-by #(.getName %))
       (map #(.getPath %))))

(defn expand-spec-file [path]
  (let [ns-prefix (-> (io/file path) .getName (str/replace #"\.edn$" ""))
        specs (edn/read-string (slurp path))]
    (vec (mapcat (fn [{:keys [category tests]}]
                   (mapv #(assoc % :category category :ns ns-prefix) tests))
                 specs))))

(defn do-expand []
  (let [files (spec-files)
        expressions (vec (mapcat expand-spec-file files))]
    (io/make-parents expressions-file)
    (spit expressions-file (pr-str expressions))
    (println (format "  %d expressions from %d spec files" (count expressions) (count files)))
    expressions))

;; =============================================================================
;; Capture — parallel eval on JVM
;; =============================================================================

(def ^:dynamic *eval-timeout-ms* 1000)
(def ^:dynamic *capture-threads* (.availableProcessors (Runtime/getRuntime)))

(defn safe-eval [expr-str]
  (let [result (promise)
        t (Thread.
            (fn []
              (deliver result
                (try
                  (binding [*print-length* 100 *print-level* 10
                            *out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
                    (let [v (eval (read-string expr-str))]
                      {:result (pr-str v) :type (str (type v))}))
                  (catch Throwable t
                    {:error (str (.getSimpleName (class t)) ": " (.getMessage t))
                     :error-class (str (.getSimpleName (class t)))})))))]
    (.setDaemon t true)
    (.start t)
    (let [r (deref result *eval-timeout-ms* ::timeout)]
      (if (= r ::timeout)
        (do (.interrupt t) {:error "TimeoutException" :error-class "TimeoutException"})
        r))))

(defn do-capture []
  (let [expressions (do-expand)
        total (count expressions)
        done (atom 0) errors (atom 0)
        start (System/currentTimeMillis)
        chunks (partition-all (max 1 (quot total *capture-threads*)) expressions)
        eval-chunk (fn [exprs]
                     (mapv (fn [{:keys [it eval category]}]
                             (let [r (safe-eval eval)
                                   n (swap! done inc)]
                               (when (:error r) (swap! errors inc))
                               (when (zero? (mod n 1000))
                                 (binding [*out* *err*]
                                   (println (format "  %d/%d (%.1fs)" n total
                                                    (/ (- (System/currentTimeMillis) start) 1000.0)))))
                               (merge {:expr eval :category category :it it} r)))
                           exprs))
        results (vec (mapcat deref (mapv #(future (eval-chunk %)) chunks)))
        elapsed (/ (- (System/currentTimeMillis) start) 1000.0)]
    (io/make-parents reference-file)
    (spit reference-file (pr-str results))
    (println (format "  %d results in %.1fs (%d values, %d errors)"
                     total elapsed (- total @errors) @errors))
    results))

(defn do-verify []
  (let [ref (edn/read-string (slurp reference-file))
        n (count ref) ok (count (remove :error ref))]
    (println (format "  %d expressions, %d values, %d errors — OK" n ok (- n ok)))))

;; =============================================================================
;; Emit parity.cljc
;; =============================================================================

(defn- esc [s]
  (-> s (str/replace "\\" "\\\\") (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n") (str/replace "\r" "\\r") (str/replace "\t" "\\t")))

(defn- portable? [{:keys [expr it result error]}]
  (and result (not error)
       (not-any? #(str/starts-with? result %) ["#object" "#<" "#error"])
       (not (str/includes? result "..."))
       (not (re-find #"gensym|rand|uuid|shuffle|thread-bind|elapsed|class.?loader"
                     (str/lower-case (str it))))
       (not (re-find #"java\.|clojure\.lang\." expr))))

(defn- core-cat? [cat]
  (let [s (name cat)]
    (and (str/starts-with? s "clojure.core")
         (not (re-find #"clojure\.core\.(reducers|server|protocols\.clojure)" s)))))

(def ^:private cljc-runner
  ";; parity.cljc — generated by clojure.parity
;; Drop into your implementation and run it.
;; Needs: def, defn, if, do, =, +, str, pr-str, println

(def _p 0) (def _f 0) (def _l nil) (def _lp 0) (def _lt 0)
(defn _r [] (if _l (println (str \"  \" _l \": \" _lp \"/\" _lt))))
(defn _b [n] (_r) (def _l n) (def _lp 0) (def _lt 0))
(defn _t [n r e] (def _lt (+ _lt 1))
  (if (= (pr-str r) e)
    (do (def _p (+ _p 1)) (def _lp (+ _lp 1)))
    (do (def _f (+ _f 1)) (println (str \"  FAIL \" n \": expected \" e \", got \" (pr-str r))))))
(println \"clojure.parity\\n\")
")

(defn emit-cljc [output-path]
  (let [ref (edn/read-string (slurp reference-file))
        tests (filter portable? ref)
        by-cat (into (sorted-map) (filter (fn [[k _]] (core-cat? k)) (group-by :category tests)))
        n (atom 0)]
    (io/make-parents output-path)
    (with-open [w (io/writer output-path)]
      (.write w cljc-runner)
      (doseq [[cat entries] by-cat]
        (.write w (str "(_b \"" (name cat) "\")\n"))
        (doseq [{:keys [expr it result]} entries]
          (.write w (str "(_t \"" (esc it) "\" " expr " \"" (esc result) "\")\n"))
          (swap! n inc))
        (.write w "\n"))
      (.write w "(_r)\n(println)\n(println (str _p \"/\" (+ _p _f) \" pass\"))\n(if (= _f 0) (println \"All tests passed.\") (println (str _f \" failures.\")))\n"))
    (println (format "  Generated %s (%d tests)" output-path @n))))

;; =============================================================================
;; Main
;; =============================================================================

(defn -main [& args]
  (let [args (vec args)
        cmd (some #{"init" "capture" "expand" "verify" "cljc"} args)
        scope (cond (some #{"--lang"} args)    :lang
                    (some #{"--contrib"} args) :contrib
                    :else                      :all)
        args (vec (remove #{"--quick" "--balanced" "--thorough" "--lang" "--contrib"
                            "init" "capture" "expand" "verify" "cljc"} args))
        pairs (partition 2 1 args)
        write-dir (some #(when (= "--write" (first %)) (second %)) pairs)
        write-args (when write-dir
                     (let [idx (.indexOf args "--write")]
                       (when (< (+ idx 2) (count args))
                         [(nth args (+ idx 1)) (nth args (+ idx 2))])))
        lang-dir (first write-args)
        contrib-dir (second write-args)
        stats-only (some #{"--stats"} args)
        ns-args (vec (remove #(str/starts-with? % "--") args))
        ns-args (reduce (fn [a d] (if d (vec (remove #{d} a)) a))
                        ns-args [lang-dir contrib-dir])
        scope-ns (case scope
                   :lang default-namespaces
                   :contrib (vec (map str contrib-namespaces))
                   :all all-namespaces)
        namespaces (if (seq ns-args) ns-args scope-ns)]
    (cond
      (= cmd "capture") (do-capture)
      (= cmd "expand")  (do-expand)
      (= cmd "verify")  (do-verify)
      (= cmd "cljc")    (emit-cljc (or (first ns-args) "parity.cljc"))

      ;; All-in-one: reflect → specs → expand → capture → verify → cljc
      (= cmd "init")
      (let [_ (load-namespaces! scope)
            ld (or lang-dir "lang/")
            cd (or contrib-dir "contrib/")
            results (mapv process-namespace namespaces)]
        (print-stats results)
        (write-specs results ld cd)
        (do-capture)
        (do-verify)
        (emit-cljc "parity.cljc"))

      ;; Spec generation only (no capture)
      :else
      (let [_ (load-namespaces! scope)
            _ (binding [*out* *err*]
                (println (format "generate: reflecting on %d namespaces..." (count namespaces))))
            results (mapv process-namespace namespaces)]
        (print-stats results)
        (when (and lang-dir contrib-dir)
          (write-specs results lang-dir contrib-dir))))))

(apply -main *command-line-args*)
