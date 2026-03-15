(ns parity.core
  "Clojure cross-compiler parity toolkit.

  Five commands:
    init     reflect -> generate -> capture -> verify
    test     compare your results against reference
    status   dashboard + what to do next
    port     rewrite JVM -> portable Clojure
    clear    remove generated files"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [parity.generate :as gen]
            [parity.port :as port]
            [parity.analyze :as analyze]))

(def results-dir "results")
(def expressions-file (str results-dir "/expressions.edn"))
(def reference-file (str results-dir "/reference.edn"))
(def results-file (str results-dir "/results.edn"))

(defn- file-exists? [path] (.exists (io/file path)))

;; =============================================================================
;; Compare — diff target results against reference
;; =============================================================================

(defn compare-results
  "Pure data comparison. Returns {:pass :fail :error :missing :failures}."
  [reference target expressions]
  (let [ref-by-expr (into {} (map (fn [r] [(:expr r) r]) reference))
        tgt-by-expr (into {} (map (fn [r] [(:expr r) r]) target))
        init-state {:pass 0 :fail 0 :error 0 :missing 0
                    :failures [] :ns-pass {} :ns-total {} :current-cat nil}
        result
        (reduce
          (fn [state {:keys [it eval category ns]}]
            (let [state (if (not= (:current-cat state) category)
                          (do (println) (println (name category))
                              (assoc state :current-cat category))
                          state)
                  state (update-in state [:ns-total ns] (fnil inc 0))
                  ref (get ref-by-expr eval)
                  tgt (get tgt-by-expr eval)]
              (cond
                (nil? tgt)
                (do (println (str "  ? " it " — missing"))
                    (update state :missing inc))

                (and (:error ref) (:error tgt))
                (if (= (:error-class ref) (:error-class tgt))
                  (do (println (str "  \u2713 " it))
                      (-> state (update :pass inc) (update-in [:ns-pass ns] (fnil inc 0))))
                  (do (println (str "  \u2717 " it " — expected " (:error-class ref) " got " (:error-class tgt)))
                      (-> state (update :fail inc)
                          (update :failures conj {:it it :expr eval :expected (:error ref) :actual (:error tgt)}))))

                (:error ref)
                (do (println (str "  \u2717 " it " — expected error, got " (:result tgt)))
                    (-> state (update :fail inc)
                        (update :failures conj {:it it :expr eval :expected (str "ERROR: " (:error ref)) :actual (:result tgt)})))

                (:error tgt)
                (do (println (str "  ! " it " — " (:error tgt)))
                    (-> state (update :error inc)
                        (update :failures conj {:it it :expr eval :expected (:result ref) :actual (str "ERROR: " (:error tgt))})))

                (= (:result ref) (:result tgt))
                (do (println (str "  \u2713 " it))
                    (-> state (update :pass inc) (update-in [:ns-pass ns] (fnil inc 0))))

                :else
                (do (println (str "  \u2717 " it " — expected " (:result ref) " got " (:result tgt)))
                    (-> state (update :fail inc)
                        (update :failures conj {:it it :expr eval :expected (:result ref) :actual (:result tgt)}))))))
          init-state
          expressions)]

    ;; Per-namespace
    (println)
    (println "=== PER-NAMESPACE ===")
    (doseq [ns-name (sort (keys (:ns-total result)))]
      (let [total (get (:ns-total result) ns-name 0)
            passed (get (:ns-pass result) ns-name 0)]
        (println (format "  %-40s %d/%d (%.1f%%)" ns-name passed total
                         (if (pos? total) (* 100.0 (/ (double passed) total)) 0.0)))))

    ;; Summary
    (println)
    (println "=== PARITY REPORT ===")
    (let [{:keys [pass fail error missing]} result
          total (+ pass fail error missing)]
      (println (format "  Pass:    %d/%d (%.1f%%)" pass total
                       (if (pos? total) (* 100.0 (/ (double pass) total)) 0.0)))
      (println (format "  Fail:    %d" fail))
      (println (format "  Error:   %d" error))
      (println (format "  Missing: %d" missing)))

    (when (seq (:failures result))
      (println)
      (println "--- Failures (first 20) ---")
      (doseq [{:keys [it expr expected actual]} (take 20 (:failures result))]
        (println (str "  " it))
        (println (str "    expr:     " expr))
        (println (str "    expected: " expected))
        (println (str "    actual:   " actual))))

    (select-keys result [:pass :fail :error :missing :failures])))

;; =============================================================================
;; init
;; =============================================================================

(defn init
  "Reflect -> generate -> capture -> verify."
  [& args]
  (let [flags    (set (filter #(str/starts-with? % "--") args))
        ns-args  (remove #(str/starts-with? % "--") args)
        tier     (cond (flags "--quick")    "--quick"
                       (flags "--thorough") "--thorough"
                       :else                "--balanced")
        scope    (cond (flags "--lang")    "--lang"
                       (flags "--contrib") "--contrib"
                       :else               nil)
        sg-args  (cond-> [tier "--write" "lang/" "contrib/"]
                   scope   (conj scope)
                   (seq ns-args) (into ns-args))]
    (println (str "=== init (" (subs tier 2)
                  (when scope (str ", " (subs scope 2)))
                  (when (seq ns-args) (str ", " (count ns-args) " namespaces"))
                  ") ==="))
    (println)
    (apply gen/-main "init" sg-args)
    (println "\nReady.")
    (println "  Quick:    your-clojure parity.cljc")
    (println "  Detailed: par test results/results.edn")))

;; =============================================================================
;; test
;; =============================================================================

(defn test-impl
  "Compare target results against JVM reference."
  [file]
  (when-not (file-exists? reference-file)
    (println "No reference found. Run: par init")
    (System/exit 1))
  (when-not (file-exists? file)
    (println (str "File not found: " file))
    (System/exit 1))
  (let [reference (edn/read-string (slurp reference-file))
        target (edn/read-string (slurp file))
        expressions (edn/read-string (slurp expressions-file))]
    (println (format "=== PARITY: %s vs reference ===\n" file))
    (compare-results reference target expressions)))

;; =============================================================================
;; status
;; =============================================================================

(defn status
  "Dashboard: coverage, pass/fail, what's next."
  [& args]
  (let [flags (set (filter #(str/starts-with? % "--") args))
        positional (vec (remove #(str/starts-with? % "--") args))]

    (cond
      (not (file-exists? "lang/"))
      (do (println "No specs generated. Run: par init") (System/exit 0))
      (not (file-exists? reference-file))
      (do (println "Specs generated but no reference. Run: par init") (System/exit 0)))

    (let [ref (edn/read-string (slurp reference-file))
          exprs (edn/read-string (slurp expressions-file))
          n (count ref)
          values (count (remove :error ref))
          errors (count (filter :error ref))
          by-ns (group-by :ns exprs)]
      (println "=== PARITY STATUS ===")
      (println)
      (println (format "  Reference: %d expressions (%d values, %d expected errors)" n values errors))
      (println (format "  Namespaces: %d" (count by-ns)))
      (println)

      (if (file-exists? results-file)
        (let [target (edn/read-string (slurp results-file))
              tgt-by-expr (into {} (map (fn [r] [(:expr r) r]) target))
              ref-by-expr (into {} (map (fn [r] [(:expr r) r]) ref))
              init-state {:pass 0 :fail 0 :error 0 :missing 0
                          :ns-pass {} :ns-total {}}
              result
              (reduce
                (fn [state {:keys [eval ns]}]
                  (let [state (update-in state [:ns-total ns] (fnil inc 0))
                        r (get ref-by-expr eval)
                        t (get tgt-by-expr eval)]
                    (cond
                      (nil? t)
                      (update state :missing inc)

                      (and (:error r) (:error t) (= (:error-class r) (:error-class t)))
                      (-> state (update :pass inc) (update-in [:ns-pass ns] (fnil inc 0)))

                      (and (not (:error r)) (not (:error t)) (= (:result r) (:result t)))
                      (-> state (update :pass inc) (update-in [:ns-pass ns] (fnil inc 0)))

                      (:error t) (update state :error inc)
                      :else      (update state :fail inc))))
                init-state
                exprs)
              {:keys [pass fail error missing ns-pass ns-total]} result]

          (let [total (+ pass fail error missing)]
            (println (format "  Results: %d/%d pass (%.1f%%)" pass total
                             (if (pos? total) (* 100.0 (/ (double pass) total)) 0.0)))
            (println (format "           %d fail, %d error, %d missing" fail error missing))
            (println)

            (println "  Per namespace:")
            (let [ns-data (for [ns-name (sort (keys ns-total))]
                            {:ns ns-name
                             :total (get ns-total ns-name 0)
                             :pass (get ns-pass ns-name 0)})
                  complete (filter #(= (:pass %) (:total %)) ns-data)
                  incomplete (sort-by #(/ (double (:pass %)) (max 1 (:total %)))
                                      (remove #(= (:pass %) (:total %)) ns-data))]
              (doseq [{:keys [ns total pass]} incomplete]
                (println (format "    %-40s %4d/%4d (%s%%)"
                                 ns pass total
                                 (format "%.0f" (* 100.0 (/ (double pass) (max 1 total)))))))
              (when (seq complete)
                (println (format "\n    Complete: %d namespaces" (count complete)))
                (println (str "    " (str/join ", " (map :ns complete)))))

              (println)
              (println "  Next wins:")
              (doseq [{:keys [ns total pass]} (take 5 (sort-by #(- (:total %) (:pass %)) >
                                                               (filter #(< (:pass %) (:total %)) ns-data)))]
                (println (format "    %-40s %d remaining" ns (- total pass)))))))

        (println "  No results.edn found. Awaiting target implementation results."))

      (when (flags "--roadmap")
        (if-let [src (first positional)]
          (do (println "\n=== ROADMAP ===")
              (analyze/roadmap src))
          (println "\n  --roadmap requires: par status --roadmap <clojure-src>")))

      (when (flags "--reflect")
        (println "\n=== JVM HOST CONTRACT ===")
        (analyze/reflect)))))

;; =============================================================================
;; clear
;; =============================================================================

(defn clear []
  (doseq [dir ["lang" "contrib" "results"]]
    (let [f (io/file dir)]
      (when (.exists f)
        (doseq [child (reverse (file-seq f))] (.delete child))
        (println (str "  removed " dir "/")))))
  (println "Clean."))

;; =============================================================================
;; CLI
;; =============================================================================

(defn usage []
  (println "
  par — Clojure cross-compiler parity toolkit

  par init [options]                 Reflect -> generate -> capture -> verify
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

  par port <in.clj> [out.cljc]       Rewrite JVM -> portable Clojure

  par clear                          Remove generated files
"))

(defn -main [& args]
  (let [[cmd & cmd-args] args]
    (case cmd
      "init"   (apply init cmd-args)
      "test"   (if (first cmd-args)
                 (test-impl (first cmd-args))
                 (do (println "Usage: par test <results.edn>") (System/exit 1)))
      "status" (apply status cmd-args)
      "port"   (port/transform (first cmd-args)
                               (or (second cmd-args)
                                   (str/replace (first cmd-args) #"\.clj$" "_portable.cljc")))
      "clear"  (clear)
      (usage))
))
