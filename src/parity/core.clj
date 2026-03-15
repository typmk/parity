(ns parity.core
  "Parity toolkit — measures how close an alternative Clojure implementation
  is to JVM Clojure, using the JVM itself as the oracle.

  Entry point for all parity operations. Each function can be called from
  the REPL or via CLI: clojure -M src/parity/core.clj <command> [args...]")

;; Load all modules. Each defines functions but won't execute (guarded by
;; *command-line-args* being nil when loaded via require).
;; We use load-file to avoid ns conflicts with the script-style files.

(require '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[clojure.edn :as edn])

;; =============================================================================
;; Specgen — generate test specs from JVM reflection
;; =============================================================================

(defn specgen
  "Generate test specs. Writes to lang-dir and contrib-dir.
   (specgen \"lang/\" \"contrib/\")
   (specgen :stats)
   (specgen \"clojure.string\" \"clojure.set\")"
  [& args]
  (load-file "src/parity/specgen.clj")
  (let [main (resolve 'parity.specgen/-main)]
    ;; specgen already ran via (apply -main *command-line-args*) on load,
    ;; but *command-line-args* was nil, so -main got nil args → prints stats.
    ;; We need to call it explicitly.
    nil))

;; The script-style modules run on load. To make them callable as functions,
;; we need to prevent auto-execution. Let's take a different approach:
;; core.clj IS the entry point. It parses args and calls the right module's
;; -main directly, loading it only when needed.

(defn- load-and-call [file & args]
  (binding [*command-line-args* (vec args)]
    (load-file file)))

;; =============================================================================
;; Generate
;; =============================================================================

(defn generate
  "Generate all specs into lang/ and contrib/ directories.
   Equivalent to: par specgen --write lang/ contrib/"
  ([] (generate "lang/" "contrib/"))
  ([lang-dir contrib-dir]
   (load-and-call "src/parity/specgen.clj" "--write" lang-dir contrib-dir)))

(defn generate-stats
  "Show spec generation statistics without writing files."
  []
  (load-and-call "src/parity/specgen.clj" "--stats"))

;; =============================================================================
;; Test pipeline
;; =============================================================================

(defn expand
  "Expand parametric spec templates into concrete test expressions.
   Reads from spec/lang/contrib dirs, writes expressions.edn"
  []
  (load-and-call "src/parity/parity.clj" "expand"))

(defn capture
  "Evaluate all expressions on JVM Clojure, save as reference.edn"
  []
  (load-and-call "src/parity/parity.clj" "capture"))

(defn test-impl
  "Compare implementation results against JVM reference.
   No args = self-test (reference vs reference).
   With path = compare target results file."
  ([] (load-and-call "src/parity/parity.clj" "test"))
  ([results-file] (load-and-call "src/parity/parity.clj" "test" results-file)))

(defn stats
  "Show spec coverage statistics."
  []
  (load-and-call "src/parity/parity.clj" "stats"))

(defn full
  "Full pipeline: generate → expand → capture → stats"
  ([] (full "lang/" "contrib/"))
  ([lang-dir contrib-dir]
   (println "=== specgen ===")
   (generate lang-dir contrib-dir)
   (println "\n=== expand ===")
   (expand)
   (println "\n=== capture ===")
   (capture)
   (println "\n=== stats ===")
   (stats)))

;; =============================================================================
;; Discover
;; =============================================================================

(defn discover
  "JVM host contract discovery via reflection.
   Returns host data map (printed to stdout)."
  [& args]
  (apply load-and-call "src/parity/langmap.clj" (or args [])))

(defn deps
  "Source-level dependency graph.
   (deps \"path/to/clojure/src\")
   (deps \"path\" \"--edn\")
   (deps \"path\" \"--host\")"
  [& args]
  (apply load-and-call "src/parity/depgraph.clj" args))

;; =============================================================================
;; Analyze
;; =============================================================================

(defn tree
  "Merged dependency tree with host contract. Requires clojure source path.
   Orchestrates depgraph + langmap + tree internally."
  [clojure-src & args]
  (let [tmp-graph (java.io.File/createTempFile "parity-graph" ".edn")
        tmp-host  (java.io.File/createTempFile "parity-host" ".edn")]
    (try
      ;; Generate graph EDN
      (let [graph-out (with-out-str
                        (load-and-call "src/parity/depgraph.clj" clojure-src "--edn"))]
        (spit tmp-graph graph-out))
      ;; Generate host EDN
      (let [host-out (with-out-str
                       (load-and-call "src/parity/langmap.clj" "--edn"))]
        (spit tmp-host host-out))
      ;; Run tree
      (apply load-and-call "src/parity/tree.clj"
             (str tmp-graph) (str tmp-host) args)
      (finally
        (.delete tmp-graph)
        (.delete tmp-host)))))

(defn coverage
  "Host coverage analysis. Cross-references ported code against JVM host surface.
   (coverage \"ported-dir/\" \"path/to/clojure/src\")"
  [ported-dir clojure-src]
  (let [tmp (java.io.File/createTempFile "parity-host" ".edn")]
    (try
      (let [host-out (with-out-str
                       (load-and-call "src/parity/depgraph.clj" clojure-src "--host-edn"))]
        (spit tmp host-out))
      (load-and-call "src/parity/specgen.clj"
                     "--coverage" ported-dir "--host-data" (str tmp))
      (finally
        (.delete tmp)))))

;; =============================================================================
;; Rewrite
;; =============================================================================

(defn port
  "Rewrite JVM-specific Clojure source to portable Clojure.
   (port \"core.clj\")
   (port \"core.clj\" \"core.cljc\")"
  [in-file & [out-file]]
  (if out-file
    (load-and-call "src/parity/portabilize.clj" in-file out-file)
    (load-and-call "src/parity/portabilize.clj" in-file)))

;; =============================================================================
;; Util
;; =============================================================================

(defn check
  "Check bracket/paren/brace balance in Clojure files."
  [& files]
  (apply load-and-call "src/parity/utils.clj" "brackets" files))

(defn forms
  "Debug-print top-level forms from a file."
  [file]
  (load-and-call "src/parity/utils.clj" "forms" file))

;; =============================================================================
;; CLI dispatch
;; =============================================================================

(defn usage []
  (println "
  Clojure Parity Toolkit

  GENERATE
    generate [lang/ contrib/]    Generate all specs (default dirs: lang/ contrib/)
    generate-stats               Stats only, no file output

  TEST
    expand                       Specs → expressions.edn
    capture                      Eval on JVM → reference.edn
    test [results.edn]           Compare against reference
    stats                        Coverage overview
    full                         generate → expand → capture → stats

  DISCOVER
    discover [--edn]             JVM host contract (reflection)
    deps <src> [--edn|--host]    Source dependency graph

  ANALYZE
    tree <src> [--edn]           Dependency tree + host contract
    coverage <ported> <src>      Host coverage analysis

  REWRITE
    port <in.clj> [out.cljc]    JVM → portable Clojure

  UTIL
    check <file...>              Bracket balance
    forms <file>                 Debug-print top-level forms

  Run via: clojure -M src/parity/core.clj <command> [args...]
"))

(defn -main [& args]
  (let [[cmd & cmd-args] args]
    (case cmd
      "generate"       (if (seq cmd-args)
                         (apply generate cmd-args)
                         (generate))
      "generate-stats" (generate-stats)
      ("gen" "specgen") (if (seq cmd-args)
                          (apply load-and-call "src/parity/specgen.clj" cmd-args)
                          (generate-stats))
      "expand"         (expand)
      "capture"        (capture)
      "test"           (if (first cmd-args) (test-impl (first cmd-args)) (test-impl))
      "stats"          (stats)
      "full"           (if (seq cmd-args) (apply full cmd-args) (full))
      "discover"       (apply discover cmd-args)
      ("lang" "langmap") (apply discover cmd-args)
      "deps"           (apply deps cmd-args)
      ("dep" "depgraph") (apply deps cmd-args)
      "host"           (apply deps "--host" cmd-args)
      "tree"           (apply tree cmd-args)
      "coverage"       (apply coverage cmd-args)
      ("cov")          (apply coverage cmd-args)
      "port"           (apply port cmd-args)
      "check"          (apply check cmd-args)
      ("br" "brackets") (apply check cmd-args)
      "forms"          (apply forms cmd-args)
      (usage))))

(apply -main *command-line-args*)
