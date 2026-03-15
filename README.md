# clojure.parity

Clojure cross-compiler parity toolkit. Measures how close an alternative Clojure implementation is to JVM Clojure — using the JVM itself as the oracle.

## The contract

Parity generates the questions. You provide the answers.

```
parity produces:
  expressions.edn  [{:expr "(+ 1 2)" :category :arithmetic :it "+ 1 2"} ...]
  reference.edn    [{:expr "(+ 1 2)" :result "3" :type "java.lang.Long"} ...]

you produce:
  results.edn      [{:expr "(+ 1 2)" :result "3"} ...]
                or [{:expr "(+ 1 2)" :error "ArityException: ..."} ...]
```

Your harness reads `expressions.edn`, evaluates each `:expr` in your runtime, and writes `results.edn`. How you eval is your problem — JVM, native binary, transpiled JS, whatever.

## Flow

```
par init          Generate reference from JVM
      |
      |           You write a harness, eval each :expr, produce results.edn
      v
par test          Compare your results against reference
      |
      v
par status        See where you stand, what's next
      |
      |           Implement more, re-run harness
      v
par test          Repeat
```

## Commands

```
par init [options]                 Reflect -> generate -> capture -> verify
  --quick                          ~2k expressions, ~2s
  --balanced                       ~9k expressions, ~5s (default)
  --thorough                       ~40k expressions, ~25s
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
# 1. Generate reference
par init

# 2. Write a harness for your target runtime (see below)
#    Read expressions.edn, eval each :expr, write results.edn

# 3. Compare
par test results.edn

# 4. See where you stand
par status
```

## Example output

```
=== PARITY STATUS ===

  Reference: 1489 expressions (1080 values, 409 expected errors)
  Namespaces: 31 lang, 0 contrib

  Results: 893/1489 pass (60.0%)
           130 fail, 93 error, 373 missing

  Per namespace:
    clojure.core                              893/1021 (87%)
    clojure.string                              0/  26 (0%)
    clojure.set                                 0/  13 (0%)
    ...

  Next wins (most tests unlocked):
    clojure.core                             128 remaining
```

## Writing a harness

A harness is ~20 lines. Read EDN, eval, write EDN.

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
par                     CLI (bash -> core.clj)
deps.edn                Clojure project deps
src/parity/
  core.clj              Entry point: init, test, status, clear + compare logic
  specgen.clj           JVM reflection -> test specs (.edn)
  generate.clj          Expand specs, capture JVM reference
  analyze.clj           Coordinator: reflect, deps, roadmap
    roots.clj            Native primitives — JVM host contract (reflection)
    branch.clj           Dependency chains — source graph (rewrite-clj)
    tree.clj             The merge — prioritized implementation roadmap
  portabilize.clj       JVM -> portable rewriter (experimental)
  color.clj             ANSI terminal helpers
lang/                   Generated: shipped Clojure specs (gitignored)
contrib/                Generated: contrib library specs (gitignored)
results/                Generated: expressions + reference (gitignored)
```

## Requirements

- Clojure 1.12+
- JVM 21+

## License

Copyright (c) Apollo Nicolson and contributors.

Distributed under the Eclipse Public License 2.0.
