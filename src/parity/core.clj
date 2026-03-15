(ns parity.core
  "Clojure cross-compiler parity toolkit.

  Measures how close an alternative Clojure implementation is to JVM Clojure,
  using the JVM itself as the oracle. Parity generates the questions — the
  compiler builder provides the answers."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

;; =============================================================================
;; Module loading
;; =============================================================================

(defn- load-and-call
  "Load a script file with the given command-line args bound."
  [file & args]
  (binding [*command-line-args* (vec args)]
    (load-file file)))

;; =============================================================================
;; Commands
;; =============================================================================

(defn init
  "One-shot setup: reflect on JVM → generate specs → capture reference answers.
   Creates lang/, contrib/, and results/ directories.
   Options: --quick (~1.5k), --balanced (default, ~5k), --thorough (~40k)
   (init)
   (init \"--quick\")"
  [& args]
  (let [tier-flags (filter #{"--quick" "--balanced" "--thorough"} args)
        tier-flag (or (first tier-flags) "--balanced")
        rest-args (remove #{"--quick" "--balanced" "--thorough"} args)
        lang-dir (or (first rest-args) "lang/")
        contrib-dir (or (second rest-args) "contrib/")]
    (println (str "=== reflect + generate (" (subs tier-flag 2) ") ==="))
    (load-and-call "src/parity/specgen.clj" tier-flag "--write" lang-dir contrib-dir)
    (println "\n=== expand ===")
    (load-and-call "src/parity/parity.clj" "expand")
    (println "\n=== capture (evaluating on JVM) ===")
    (load-and-call "src/parity/parity.clj" "capture")
    (println "\nDone. Reference captured in results/reference.edn")
    (println "Ship expressions.edn to your target runtime, eval each :expr,")
    (println "write results.edn, then run: par test results.edn")))

(defn test-impl
  "Compare target results against JVM reference.
   No args = self-test (reference vs reference).
   (test-impl)
   (test-impl \"path/to/results.edn\")"
  ([] (load-and-call "src/parity/parity.clj" "test"))
  ([results-file] (load-and-call "src/parity/parity.clj" "test" results-file)))

(defn status
  "Dashboard: spec coverage, pass/fail counts, what's next."
  []
  (load-and-call "src/parity/parity.clj" "stats"))

(defn reflect
  "JVM host contract discovery via reflection.
   Shows what interfaces, methods, and types Clojure requires from the host.
   (reflect)
   (reflect \"--edn\")"
  [& args]
  (apply load-and-call "src/parity/langmap.clj" (or args [])))

(defn deps
  "Source-level dependency graph from Clojure .clj files.
   (deps \"path/to/clojure/src\")
   (deps \"path\" \"--edn\")
   (deps \"path\" \"--host\")"
  [& args]
  (apply load-and-call "src/parity/depgraph.clj" args))

(defn roadmap
  "What to implement next. Merges source deps + JVM host contract into a
   prioritized implementation roadmap.
   (roadmap \"path/to/clojure/src\")"
  [clojure-src & args]
  (let [tmp-graph (java.io.File/createTempFile "parity-graph" ".edn")
        tmp-host  (java.io.File/createTempFile "parity-host" ".edn")]
    (try
      (let [graph-out (with-out-str
                        (load-and-call "src/parity/depgraph.clj" clojure-src "--edn"))]
        (spit tmp-graph graph-out))
      (let [host-out (with-out-str
                       (load-and-call "src/parity/langmap.clj" "--edn"))]
        (spit tmp-host host-out))
      (apply load-and-call "src/parity/tree.clj"
             (str tmp-graph) (str tmp-host) args)
      (finally
        (.delete tmp-graph)
        (.delete tmp-host)))))

(defn coverage
  "What's been ported vs what remains. Cross-references ported .cljc files
   against JVM host surface.
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

(defn port
  "Rewrite JVM-specific Clojure source to portable Clojure.
   (port \"core.clj\")
   (port \"core.clj\" \"core.cljc\")"
  [in-file & [out-file]]
  (if out-file
    (load-and-call "src/parity/portabilize.clj" in-file out-file)
    (load-and-call "src/parity/portabilize.clj" in-file)))

(defn clear
  "Remove all generated files (lang/, contrib/, results/)."
  []
  (doseq [dir ["lang" "contrib" "results"]]
    (let [f (io/file dir)]
      (when (.exists f)
        (doseq [child (reverse (file-seq f))]
          (.delete child))
        (println (str "  removed " dir "/")))))
  (println "Clean."))

(defn check
  "Check bracket/paren/brace balance in Clojure files."
  [& files]
  (apply load-and-call "src/parity/utils.clj" "brackets" files))

(defn forms
  "Debug-print top-level forms from a file."
  [file]
  (load-and-call "src/parity/utils.clj" "forms" file))

;; =============================================================================
;; CLI
;; =============================================================================

(defn usage []
  (println "
  par — Clojure cross-compiler parity toolkit

  SETUP
    par init                       Reflect → generate → capture (~5k expressions)
    par init --quick               Happy path only (~1.5k expressions)
    par init --thorough            Full cross-product (~40k expressions)

  TEST
    par test                       Self-check (reference vs reference)
    par test <results.edn>         Compare target impl against reference
    par status                     Coverage dashboard

  DISCOVER
    par reflect                    JVM host contract (what to implement)
    par reflect --edn              Machine-readable output
    par deps <src>                 Source dependency graph
    par deps <src> --edn           Machine-readable graph

  ANALYZE
    par roadmap <src>              What to implement next (prioritized)
    par coverage <ported> <src>    What's been ported vs what remains

  REWRITE
    par port <in.clj> [out.cljc]   JVM → portable Clojure (experimental)

  UTIL
    par clear                      Remove generated files (lang/, contrib/, results/)
"))

(defn -main [& args]
  (let [[cmd & cmd-args] args]
    (case cmd
      "init"     (apply init cmd-args)
      "test"     (if (first cmd-args) (test-impl (first cmd-args)) (test-impl))
      "status"   (status)
      "reflect"  (apply reflect cmd-args)
      "deps"     (apply deps cmd-args)
      "roadmap"  (apply roadmap cmd-args)
      "coverage" (apply coverage cmd-args)
      "port"     (apply port cmd-args)
      "clear"    (clear)
      "check"    (apply check cmd-args)
      "forms"    (apply forms cmd-args)
      (usage))))

(apply -main *command-line-args*)
