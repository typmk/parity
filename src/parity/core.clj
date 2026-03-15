(ns parity.core
  "Clojure cross-compiler parity toolkit.

  Measures how close an alternative Clojure implementation is to JVM Clojure,
  using the JVM itself as the oracle. Parity generates the questions — the
  compiler builder provides the answers.

  Four commands:
    init     reflect → generate → capture → verify
    test     compare your results against reference
    status   dashboard + what to do next
    clear    remove generated files"
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
;; Paths
;; =============================================================================

(def results-dir "results")
(def expressions-file (str results-dir "/expressions.edn"))
(def reference-file (str results-dir "/reference.edn"))

(defn- file-exists? [path] (.exists (io/file path)))

;; =============================================================================
;; init
;; =============================================================================

(defn init
  "One-shot setup: reflect → generate → capture → self-verify.

   Tiers:  --quick (~2k)  --balanced (~9k, default)  --thorough (~40k)
   Scope:  --lang (shipped only)  --contrib (contrib only)  [ns...] (specific)

   (init)
   (init \"--quick\")
   (init \"--lang\")
   (init \"--quick\" \"--lang\")
   (init \"clojure.core\" \"clojure.string\")"
  [& args]
  (let [flags    (set (filter #(str/starts-with? % "--") args))
        ns-args  (remove #(str/starts-with? % "--") args)
        tier     (cond (flags "--quick")    "--quick"
                       (flags "--thorough") "--thorough"
                       :else                "--balanced")
        scope    (cond (flags "--lang")    "--lang"
                       (flags "--contrib") "--contrib"
                       :else               nil)
        ;; Build specgen args
        sg-args  (cond-> [tier "--write" "lang/" "contrib/"]
                   scope   (conj scope)
                   (seq ns-args) (into ns-args))]
    (println (str "=== reflect + generate (" (subs tier 2)
                  (when scope (str ", " (subs scope 2)))
                  (when (seq ns-args) (str ", " (count ns-args) " namespaces"))
                  ") ==="))
    (apply load-and-call "src/parity/specgen.clj" sg-args)
    (println "\n=== expand ===")
    (load-and-call "src/parity/parity.clj" "expand")
    (println "\n=== capture ===")
    (load-and-call "src/parity/parity.clj" "capture")
    (println "\n=== verify ===")
    (let [ref (edn/read-string (slurp reference-file))
          n (count ref)
          ok (count (remove :error ref))]
      (println (format "  %d expressions, %d values, %d errors — reference OK" n ok (- n ok))))
    (println "\nReady. Ship expressions.edn to your target, eval each :expr,")
    (println "write results.edn, then run: par test results.edn")))

;; =============================================================================
;; test
;; =============================================================================

(defn test-impl
  "Compare target results.edn against JVM reference.
   (test-impl \"results.edn\")"
  [results-file]
  (when-not (file-exists? reference-file)
    (println "No reference found. Run: par init")
    (System/exit 1))
  (when-not (file-exists? results-file)
    (println (str "File not found: " results-file))
    (System/exit 1))
  (load-and-call "src/parity/parity.clj" "test" results-file))

;; =============================================================================
;; status
;; =============================================================================

(defn status
  "Dashboard: what's generated, what passes, what's next.

   --roadmap <src>   include implementation priority from source analysis
   --reflect         include JVM host contract summary

   (status)
   (status \"--roadmap\" \"path/to/clojure/src\")"
  [& args]
  (let [flags (set (filter #(str/starts-with? % "--") args))
        positional (remove #(str/starts-with? % "--") args)]

    ;; === State check ===
    (cond
      (not (file-exists? "lang/"))
      (do (println "No specs generated. Run: par init")
          (System/exit 0))

      (not (file-exists? reference-file))
      (do (println "Specs generated but no reference captured. Run: par init")
          (System/exit 0)))

    ;; === Reference summary ===
    (let [ref (edn/read-string (slurp reference-file))
          exprs (edn/read-string (slurp expressions-file))
          n (count ref)
          values (count (remove :error ref))
          errors (count (filter :error ref))
          by-ns (group-by :ns exprs)
          lang-count (count (filter (fn [[ns _]] (and ns (not (re-find #"(core\.async|core\.cache|core\.memoize|core\.logic|core\.match|core\.unify|data\.csv|data\.json|data\.xml|data\.zip|data\.priority|data\.int-map|data\.avl|java\.data|java\.classpath|math\.combinatorics|math\.numeric|test\.check|tools\.cli|tools\.logging|tools\.namespace|tools\.reader|tools\.trace)" (str ns))))) by-ns))
          contrib-count (- (count by-ns) lang-count)]
      (println "=== PARITY STATUS ===")
      (println)
      (println (format "  Reference: %d expressions (%d values, %d expected errors)" n values errors))
      (println (format "  Namespaces: %d lang, %d contrib" lang-count contrib-count))
      (println)

      ;; === Results comparison (if results.edn exists) ===
      (if (file-exists? "results/results.edn")
        (let [target (edn/read-string (slurp "results/results.edn"))
              tgt-by-expr (into {} (map (fn [r] [(:expr r) r]) target))
              ref-by-expr (into {} (map (fn [r] [(:expr r) r]) ref))
              pass (atom 0) fail (atom 0) error (atom 0) missing (atom 0)
              ns-pass (atom {}) ns-total (atom {})]
          (doseq [{:keys [eval ns] :as expr} exprs]
            (let [r (get ref-by-expr eval)
                  t (get tgt-by-expr eval)]
              (swap! ns-total update ns (fnil inc 0))
              (cond
                (nil? t)                          (swap! missing inc)
                (and (:error r) (:error t)
                     (= (:error-class r)
                        (:error-class t)))        (do (swap! pass inc)
                                                      (swap! ns-pass update ns (fnil inc 0)))
                (and (not (:error r)) (not (:error t))
                     (= (:result r) (:result t))) (do (swap! pass inc)
                                                       (swap! ns-pass update ns (fnil inc 0)))
                (:error t)                         (swap! error inc)
                :else                              (swap! fail inc))))

          (let [total (+ @pass @fail @error @missing)
                pct (if (pos? total) (format "%.1f" (* 100.0 (/ (double @pass) total))) "0.0")]
            (println (format "  Results: %s/%d pass (%s%%)" @pass total pct))
            (println (format "           %d fail, %d error, %d missing" @fail @error @missing))
            (println)

            ;; Per-namespace breakdown
            (println "  Per namespace:")
            (let [ns-data (for [ns-name (sort (keys @ns-total))]
                            {:ns ns-name
                             :total (get @ns-total ns-name 0)
                             :pass (get @ns-pass ns-name 0)})
                  complete (filter #(= (:pass %) (:total %)) ns-data)
                  incomplete (remove #(= (:pass %) (:total %)) ns-data)
                  incomplete-sorted (sort-by #(- (/ (double (:pass %)) (max 1 (:total %)))) incomplete)]

              ;; Show incomplete first, sorted by % ascending
              (doseq [{:keys [ns total pass]} incomplete-sorted]
                (let [pct (format "%.0f" (* 100.0 (/ (double pass) (max 1 total))))]
                  (println (format "    %-40s %4d/%4d (%s%%)" ns pass total pct))))

              ;; Completed namespaces as summary
              (when (seq complete)
                (println (format "\n    Complete: %d namespaces" (count complete)))
                (println (str "    " (str/join ", " (map :ns complete)))))

              ;; Next wins
              (println)
              (println "  Next wins (most tests unlocked):")
              (let [near-complete (filter #(and (pos? (:total %))
                                               (> (:pass %) 0)
                                               (< (:pass %) (:total %)))
                                         ns-data)
                    by-remaining (sort-by #(- (:total %) (:pass %)) near-complete)]
                (doseq [{:keys [ns total pass]} (take 5 by-remaining)]
                  (println (format "    %-40s %d remaining" ns (- total pass))))))))

        ;; No results yet
        (println "  No results.edn found. Awaiting target implementation results.")))

    ;; === Roadmap (if requested) ===
    (when (flags "--roadmap")
      (let [src (first positional)]
        (when-not src
          (println "\n  --roadmap requires a source path: par status --roadmap <clojure-src>")
          (System/exit 1))
        (println "\n=== ROADMAP ===")
        (let [tmp-graph (java.io.File/createTempFile "parity-graph" ".edn")
              tmp-host  (java.io.File/createTempFile "parity-host" ".edn")]
          (try
            (let [graph-out (with-out-str
                              (load-and-call "src/parity/depgraph.clj" src "--edn"))]
              (spit tmp-graph graph-out))
            (let [host-out (with-out-str
                             (load-and-call "src/parity/langmap.clj" "--edn"))]
              (spit tmp-host host-out))
            (load-and-call "src/parity/tree.clj" (str tmp-graph) (str tmp-host))
            (finally
              (.delete tmp-graph)
              (.delete tmp-host))))))

    ;; === Reflect (if requested) ===
    (when (flags "--reflect")
      (println "\n=== JVM HOST CONTRACT ===")
      (load-and-call "src/parity/langmap.clj"))))

;; =============================================================================
;; clear
;; =============================================================================

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

;; =============================================================================
;; Standalone utilities (not in main CLI, available from REPL or direct call)
;; =============================================================================

(defn port
  "Rewrite JVM-specific Clojure source to portable Clojure."
  [in-file & [out-file]]
  (if out-file
    (load-and-call "src/parity/portabilize.clj" in-file out-file)
    (load-and-call "src/parity/portabilize.clj" in-file)))

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

  par init [options]                 Reflect → generate → capture → verify
    --quick                          ~2k expressions (happy path)
    --balanced                       ~9k expressions (default)
    --thorough                       ~40k expressions (full cross-product)
    --lang                           shipped Clojure namespaces only
    --contrib                        contrib libraries only
    [ns...]                          specific namespaces

  par test <results.edn>             Compare your implementation against reference

  par status                         Dashboard: coverage, pass/fail, what's next
    --roadmap <clojure-src>          include implementation priority order
    --reflect                        include JVM host contract

  par clear                          Remove generated files
"))

(defn -main [& args]
  (let [[cmd & cmd-args] args]
    (case cmd
      "init"   (apply init cmd-args)
      "test"   (if (first cmd-args)
                 (test-impl (first cmd-args))
                 (do (println "Usage: par test <results.edn>")
                     (System/exit 1)))
      "status" (apply status cmd-args)
      "clear"  (clear)
      (usage))))

(apply -main *command-line-args*)
