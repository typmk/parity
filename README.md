# clojure.parity

Clojure cross-compiler parity toolkit. Measures how close an alternative Clojure implementation is to JVM Clojure — using the JVM itself as the oracle.

## The contract

Parity generates the questions. You provide the answers.

```
parity produces:
  expressions.edn  →  [{:expr "(+ 1 2)" :category :arithmetic :it "+ 1 2"} ...]
  reference.edn    →  [{:expr "(+ 1 2)" :result "3" :type "java.lang.Long"} ...]

you produce:
  results.edn      →  [{:expr "(+ 1 2)" :result "3"} ...]
                   or  [{:expr "(+ 1 2)" :error "ArityException: ..."} ...]
```

Your harness reads `expressions.edn`, evaluates each `:expr` in your runtime, and writes `results.edn`. How you eval is your problem — JVM, native binary, transpiled JS, whatever. Parity compares and reports.

## Commands

```
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
```

## Quick start

```bash
# 1. Generate reference (once)
par init

# 2. Write a harness for your target (see below)

# 3. Compare
par test results.edn

# 4. See what's left
par status
```

## Writing a harness

A harness is ~20 lines in any language. Read EDN, eval, write EDN.

```clojure
(let [exprs (edn/read-string (slurp "expressions.edn"))
      results (mapv (fn [{:keys [expr]}]
                      (try
                        {:expr expr :result (pr-str (eval (read-string expr)))}
                        (catch Exception e
                          {:expr expr :error (str (class e) ": " (.getMessage e))})))
                    exprs)]
  (spit "results.edn" (pr-str results)))
```

## Layout

```
par                     CLI (bash → core.clj)
deps.edn                Clojure project deps
src/parity/
  core.clj              Entry point — init, test, status, clear
  specgen.clj           JVM reflection → test specs
  parity.clj            Expand, capture, compare
  depgraph.clj          Source dependency graph
  langmap.clj           JVM host contract discovery
  tree.clj              Merged dependency tree + roadmap
  portabilize.clj       JVM → portable rewriter
  color.clj             ANSI terminal helpers
lang/                   Generated: Clojure runtime specs (gitignored)
contrib/                Generated: contrib library specs (gitignored)
results/                Generated: expressions + reference (gitignored)
```

## Requirements

- Clojure 1.12+
- JVM 21+

## License

Copyright (c) Apollo Nicolson and contributors.

Distributed under the Eclipse Public License 2.0.
