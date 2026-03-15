#!/usr/bin/env clojure
;; =============================================================================
;; specgen.clj — auto-generate parity specs from live JVM reflection
;;
;; Every Clojure var carries its own spec: arglists, doc, tag.
;; This script reflects on loaded namespaces and generates test expressions.
;; The JVM IS the oracle — run, capture, compare.
;;
;; Usage:
;;   par specgen                                           # core overview
;;   par specgen clojure.string clojure.set                # specific namespaces
;;   par init                                              # write to lang/ and contrib/
;;   par specgen --stats                                   # stats only
;;   par specgen --coverage <dir> --host-data <host.edn>   # coverage analysis
;;   par coverage <ported-dir> <clojure-src>               # (par wires both)
;; =============================================================================

(require '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[clojure.edn :as edn]
         '[clojure.pprint])

;; Load all namespaces — some may fail if deps aren't on classpath
(doseq [ns-sym '[;; Shipped with Clojure
                 clojure.set clojure.walk clojure.data clojure.datafy
                 clojure.edn clojure.math clojure.repl clojure.test
                 clojure.zip clojure.xml clojure.stacktrace clojure.template
                 clojure.instant clojure.reflect clojure.main
                 clojure.java.browse clojure.java.javadoc clojure.java.process
                 clojure.java.shell clojure.core.reducers clojure.core.protocols
                 clojure.core.server clojure.spec.alpha clojure.spec.gen.alpha
                 clojure.spec.test.alpha
                 ;; Contrib — active
                 clojure.core.async
                 clojure.core.cache clojure.core.memoize
                 clojure.data.csv clojure.data.json clojure.data.xml
                 clojure.data.zip clojure.data.priority-map
                 clojure.data.int-map clojure.data.avl clojure.data.codec.base64
                 clojure.java.data clojure.java.classpath
                 clojure.math.combinatorics clojure.math.numeric-tower
                 clojure.test.check clojure.test.check.generators
                 clojure.test.check.properties clojure.test.check.clojure-test
                 clojure.tools.cli clojure.tools.logging
                 clojure.tools.namespace.find clojure.tools.namespace.repl
                 clojure.tools.reader clojure.tools.reader.edn
                 clojure.tools.trace
                 ;; Contrib — stable
                 clojure.algo.generic.arithmetic clojure.algo.generic.collection
                 clojure.algo.generic.functor clojure.algo.generic.math-functions
                 clojure.algo.monads
                 clojure.core.logic clojure.core.match
                 clojure.core.unify]]
  (try (require ns-sym) (catch Exception _)))

;; =============================================================================
;; Value pools — "interesting values" for each domain
;; =============================================================================

;; Granularity tiers:
;;   :quick     — one well-typed value per arg, no cross-product (~1.5k exprs)
;;   :balanced  — 2-3 values + nil/empty per arg, light cross-product (~5k exprs)
;;   :thorough  — full pools, full cross-product (~40k exprs)

(def ^:dynamic *tier* :balanced)

(def pools-by-tier
  {:quick
   {:int     ["0" "42"]
    :float   ["3.14"]
    :num     ["0" "42"]
    :string  ["\"hello\""]
    :keyword [":a"]
    :symbol  ["'foo"]
    :vec     ["[1 2 3]"]
    :map     ["{:a 1}"]
    :set     ["#{1 2 3}"]
    :list    ["'(1 2 3)"]
    :coll    ["[1 2 3]"]
    :fn      ["inc"]
    :pred    ["even?"]
    :regex   ["#\"\\\\d+\""]
    :any     ["nil" "42" "\"hello\"" ":a" "[1 2]" "{:a 1}"]
    :xf      ["(map inc)"]
    :char    ["\\a"]}

   :balanced
   {:int     ["0" "1" "-1" "42"]
    :float   ["0.0" "3.14" "-2.5"]
    :num     ["0" "1" "-1" "42" "3.14"]
    :string  ["\"\"" "\"hello\"" "\"hello world\""]
    :keyword [":a" ":b" ":bar/baz"]
    :symbol  ["'foo" "'bar/baz"]
    :vec     ["[]" "[1 2 3]"]
    :map     ["{}" "{:a 1}" "{:a 1 :b 2 :c 3}"]
    :set     ["#{}" "#{1 2 3}"]
    :list    ["'()" "'(1 2 3)"]
    :coll    ["nil" "[]" "[1 2 3]" "{:a 1}" "#{1 2 3}"]
    :fn      ["inc" "identity" "str"]
    :pred    ["even?" "nil?" "string?"]
    :regex   ["#\"\\\\d+\"" "#\"[a-z]+\""]
    :any     ["nil" "0" "42" "3.14" "\"hello\"" ":a" "[]" "[1 2]" "{:a 1}" "#{1}"]
    :xf      ["(map inc)" "(filter even?)"]
    :char    ["\\a" "\\space"]}

   :thorough
   {:int     ["0" "1" "-1" "42" "100" "Long/MAX_VALUE" "Long/MIN_VALUE"]
    :float   ["0.0" "3.14" "-2.5" "Double/NaN" "Double/POSITIVE_INFINITY"]
    :num     ["0" "1" "-1" "42" "3.14"]
    :string  ["\"\"" "\"hello\"" "\"hello world\"" "\"abc\""]
    :keyword [":a" ":b" ":foo" ":bar/baz"]
    :symbol  ["'foo" "'bar" "'baz/quux"]
    :vec     ["[]" "[1]" "[1 2 3]" "[1 2 3 4 5]" "[nil]"]
    :map     ["{}" "{:a 1}" "{:a 1 :b 2 :c 3}" "{nil nil}"]
    :set     ["#{}" "#{1 2 3}" "#{:a :b}" "#{{:a 1 :b 2} {:a 3 :b 4}}"]
    :list    ["'()" "'(1 2 3)" "'(nil)"]
    :coll    ["nil" "[]" "[1 2 3]" "'(1 2 3)" "{:a 1 :b 2}" "#{1 2 3}"]
    :fn      ["inc" "dec" "identity" "str" "keyword"]
    :pred    ["even?" "odd?" "nil?" "pos?" "string?"]
    :regex   ["#\"\\\\d+\"" "#\"[a-z]+\"" "#\"hello\""]
    :any     ["nil" "true" "false" "0" "1" "-1" "42" "3.14"
              "\"\"" "\"hello\"" ":a" "'foo" "\\a"
              "[]" "[1 2]" "'()" "'(1 2)" "{}" "{:a 1}" "#{}" "#{1}"]
    :xf      ["(map inc)" "(filter even?)" "(take 3)" "(dedupe)"]
    :char    ["\\a" "\\space" "\\newline" "\\tab"]}})

(defn pools [] (get pools-by-tier *tier*))

;; =============================================================================
;; Semantic categories for clojure.core vars
;; =============================================================================

(def core-categories
  "Map of category keyword → set of var names (symbols)."
  {:literals    #{'true? 'false? 'nil? 'some? 'boolean 'char 'byte 'short 'int 'long 'float 'double
                  'bigint 'biginteger 'bigdec 'num 'rationalize}
   :arithmetic  #{'+ '- '* '/ 'quot 'rem 'mod 'inc 'dec 'max 'min 'abs
                  '+' '-' '*' 'inc' 'dec' 'unchecked-add 'unchecked-subtract
                  'unchecked-multiply 'unchecked-inc 'unchecked-dec
                  'unchecked-negate 'unchecked-remainder-int 'unchecked-divide-int}
   :comparison  #{'= '== 'not= '< '> '<= '>= 'compare 'identical? 'zero? 'pos? 'neg?
                  'pos-int? 'neg-int? 'nat-int? 'even? 'odd? 'number? 'integer? 'float?
                  'decimal? 'ratio? 'rational? 'infinite? 'NaN?}
   :bitops      #{'bit-and 'bit-or 'bit-xor 'bit-not 'bit-flip 'bit-set 'bit-clear
                  'bit-test 'bit-shift-left 'bit-shift-right 'unsigned-bit-shift-right
                  'bit-and-not}
   :boolean-logic #{'and 'or 'not 'true? 'false? 'some? 'nil? 'boolean 'if-not 'when-not
                    'not-every? 'not-any? 'not-empty}
   :strings     #{'str 'subs 'name 'namespace 'symbol 'keyword 'char 'format 'pr-str 'prn-str
                  'print-str 'with-out-str 'string? 'char? 'keyword? 'symbol? 'simple-keyword?
                  'qualified-keyword? 'simple-symbol? 'qualified-symbol? 'simple-ident?
                  'qualified-ident? 'ident?}
   :regex       #{'re-find 're-matches 're-seq 're-groups 're-pattern 're-matcher}
   :vectors     #{'vector 'vec 'vector-of 'subvec 'mapv 'filterv 'into-array 'to-array 'to-array-2d
                  'aclone 'alength 'aget 'aset 'amap 'areduce 'make-array 'vector?}
   :maps        #{'hash-map 'sorted-map 'sorted-map-by 'array-map 'zipmap 'frequencies 'group-by
                  'assoc 'dissoc 'get 'get-in 'assoc-in 'update 'update-in 'select-keys
                  'merge 'merge-with 'keys 'vals 'find 'contains? 'map? 'record?
                  'associative? 'sorted?}
   :sets        #{'set 'hash-set 'sorted-set 'sorted-set-by 'disj 'set?}
   :lists       #{'list 'list* 'list? 'cons 'conj 'peek 'pop}
   :sequences   #{'seq 'sequence 'first 'rest 'next 'second 'last 'butlast 'nth 'nthrest 'nthnext
                  'take 'take-while 'take-last 'take-nth 'drop 'drop-while 'drop-last
                  'map 'mapcat 'filter 'remove 'keep 'keep-indexed
                  'reduce 'reduce-kv 'reductions 'apply 'some 'every?
                  'concat 'flatten 'interleave 'interpose 'reverse 'sort 'sort-by
                  'distinct 'dedupe 'partition 'partition-by 'partition-all
                  'split-at 'split-with 'count 'empty 'empty? 'seq?
                  'coll? 'sequential? 'counted? 'reversible? 'seqable?}
   :lazy-sequences #{'lazy-seq 'iterate 'repeat 'repeatedly 'cycle 'range
                     'doall 'dorun 'realized? 'line-seq 'tree-seq}
   :transducers #{'transduce 'into 'eduction 'cat 'halt-when 'completing
                  'unreduced 'reduced 'reduced? 'ensure-reduced}
   :transients  #{'transient 'persistent! 'conj! 'assoc! 'dissoc! 'pop! 'disj!}
   :functions   #{'fn? 'ifn? 'comp 'partial 'juxt 'complement 'constantly 'fnil
                  'every-pred 'some-fn 'memoize 'trampoline 'apply}
   :macros      #{'-> '->> 'as-> 'cond-> 'cond->> 'some-> 'some->> 'doto
                  'when 'when-let 'when-first 'when-some 'if-let 'if-some
                  'cond 'condp 'case 'while 'for 'doseq 'dotimes 'loop 'recur
                  'let 'letfn 'fn 'defn 'defn- 'defmacro 'defmethod 'defmulti}
   :metadata    #{'meta 'with-meta 'vary-meta 'alter-meta! 'reset-meta!}
   :atoms       #{'atom 'deref 'reset! 'swap! 'compare-and-set! 'swap-vals! 'reset-vals!
                  'add-watch 'remove-watch 'set-validator! 'get-validator}
   :volatiles   #{'volatile! 'vreset! 'vswap!}
   :refs        #{'ref 'deref 'dosync 'commute 'alter 'ref-set 'ensure 'ref-history-count
                  'ref-min-history 'ref-max-history}
   :agents      #{'agent 'send 'send-off 'send-via 'await 'await-for
                  'agent-error 'restart-agent 'set-error-handler! 'set-error-mode!
                  'error-handler 'error-mode 'release-pending-sends}
   :concurrency #{'future 'future-call 'future? 'future-done? 'future-cancel 'future-cancelled?
                  'deref 'promise 'deliver 'delay 'delay? 'force
                  'locking 'pcalls 'pmap 'pvalues 'realized?}
   :namespaces  #{'ns 'in-ns 'create-ns 'remove-ns 'find-ns 'all-ns 'the-ns
                  'ns-name 'ns-publics 'ns-interns 'ns-refers 'ns-imports
                  'ns-map 'ns-aliases 'ns-resolve 'resolve}
   :type-system #{'type 'class 'supers 'bases 'ancestors 'descendants 'parents
                  'isa? 'derive 'underive 'make-hierarchy
                  'instance? 'satisfies? 'extends? 'extenders}
   :protocols   #{'defprotocol 'extend-protocol 'extend-type 'extend 'reify}
   :records-types #{'defrecord 'deftype 'defstruct 'struct-map 'struct 'create-struct
                    'accessor}
   :multimethods #{'defmulti 'defmethod 'prefer-method 'prefers 'methods
                   'get-method 'remove-method 'remove-all-methods}
   :special-forms #{'do 'if 'let 'quote 'var 'fn 'loop 'recur 'throw 'try
                    'catch 'finally 'new 'set! 'monitor-enter 'monitor-exit}
   :error-handling #{'try 'catch 'finally 'throw 'ex-info 'ex-message 'ex-data 'ex-cause
                     'Throwable->map}
   :printing    #{'pr 'prn 'print 'println 'pr-str 'prn-str 'print-str 'println-str
                  'with-out-str 'newline 'flush 'printf 'format}
   :coercion    #{'int 'long 'float 'double 'short 'byte 'char 'boolean
                  'bigint 'biginteger 'bigdec 'num 'rationalize}
   :sorted-collections #{'sorted-map 'sorted-map-by 'sorted-set 'sorted-set-by
                         'sorted? 'rseq 'subseq 'rsubseq 'compare}
   :threading   #{'-> '->> 'as-> 'cond-> 'cond->> 'some-> 'some->>}
   :destructuring #{'let 'fn 'defn 'loop 'for 'doseq}
   :dynamic-vars #{'binding 'with-bindings 'with-redefs 'thread-bound?
                   'push-thread-bindings 'pop-thread-bindings 'get-thread-bindings
                   'bound-fn 'bound-fn* 'bound?}
   :arrays      #{'make-array 'into-array 'to-array 'to-array-2d
                  'aclone 'alength 'aget 'aset 'amap 'areduce
                  'boolean-array 'byte-array 'char-array 'short-array
                  'int-array 'long-array 'float-array 'double-array 'object-array
                  'booleans 'bytes 'chars 'shorts 'ints 'longs 'floats 'doubles}})

(def var->category
  "Reverse map: var symbol → category keyword."
  (into {} (for [[cat vars] core-categories, v vars] [v cat])))

;; =============================================================================
;; Edge cases — per-function explicit tests (boundary values, nil, empty, etc.)
;; =============================================================================

(def edge-cases
  "Map of qualified fn name → vector of {:it name :eval expr} tests."
  {;; Arithmetic edge cases
   "+"         [{:it "+ no args"          :eval "(+)"}
                {:it "+ overflow"          :eval "(+ Long/MAX_VALUE 1)"}]
   "-"         [{:it "- zero"             :eval "(- 0)"}
                {:it "- underflow"         :eval "(- Long/MIN_VALUE 1)"}]
   "*"         [{:it "* no args"          :eval "(*)"}
                {:it "* by zero"           :eval "(* 42 0)"}]
   "/"         [{:it "/ by 1"             :eval "(/ 42 1)"}
                {:it "/ ratio"             :eval "(/ 1 3)"}
                {:it "/ double"            :eval "(/ 1.0 3.0)"}]
   "quot"      [{:it "quot neg"           :eval "(quot -7 2)"}]
   "rem"       [{:it "rem neg"            :eval "(rem -7 2)"}]
   "mod"       [{:it "mod neg"            :eval "(mod -7 2)"}]
   "inc"       [{:it "inc Long/MAX_VALUE" :eval "(inc' Long/MAX_VALUE)"}]
   "dec"       [{:it "dec Long/MIN_VALUE" :eval "(dec' Long/MIN_VALUE)"}]

   ;; Comparison edge cases
   "="         [{:it "= nil nil"          :eval "(= nil nil)"}
                {:it "= cross-type"       :eval "(= 1 1.0)"}
                {:it "= vector list"      :eval "(= [1 2 3] '(1 2 3))"}
                {:it "= map order"        :eval "(= {:a 1 :b 2} {:b 2 :a 1})"}
                {:it "= NaN"              :eval "(= Double/NaN Double/NaN)"}
                {:it "= empty colls"      :eval "(= [] '() #{})"}]
   "=="        [{:it "== int float"       :eval "(== 1 1.0)"}
                {:it "== ratio"           :eval "(== 1/2 0.5)"}]
   "compare"   [{:it "compare nil"        :eval "(compare nil nil)"}
                {:it "compare strings"    :eval "(compare \"a\" \"b\")"}
                {:it "compare vecs"       :eval "(compare [1 2] [1 3])"}]

   ;; Collection edge cases
   "conj"      [{:it "conj nil"           :eval "(conj nil 1)"}
                {:it "conj vec"           :eval "(conj [1 2] 3)"}
                {:it "conj map"           :eval "(conj {:a 1} [:b 2])"}
                {:it "conj set"           :eval "(conj #{1 2} 3)"}
                {:it "conj list"          :eval "(conj '(1 2) 3)"}]
   "assoc"     [{:it "assoc nil"          :eval "(assoc nil :a 1)"}
                {:it "assoc nested"       :eval "(assoc {} :a 1 :b 2 :c 3)"}]
   "get"       [{:it "get nil"            :eval "(get nil :a)"}
                {:it "get default"        :eval "(get {} :a :not-found)"}
                {:it "get vector"         :eval "(get [1 2 3] 1)"}
                {:it "get string"         :eval "(get \"hello\" 1)"}]
   "dissoc"    [{:it "dissoc missing"     :eval "(dissoc {:a 1} :b)"}
                {:it "dissoc nil"         :eval "(dissoc nil :a)"}]
   "merge"     [{:it "merge nil"          :eval "(merge nil {:a 1})"}
                {:it "merge overlap"      :eval "(merge {:a 1 :b 2} {:b 3 :c 4})"}]
   "into"      [{:it "into nil"           :eval "(into nil [1 2])"}
                {:it "into map"           :eval "(into {} [[:a 1] [:b 2]])"}
                {:it "into sorted"        :eval "(into (sorted-set) [3 1 2])"}]

   ;; Seq edge cases
   "first"     [{:it "first nil"          :eval "(first nil)"}
                {:it "first empty"        :eval "(first [])"}
                {:it "first string"       :eval "(first \"hello\")"}
                {:it "first map"          :eval "(first {:a 1 :b 2})"}]
   "rest"      [{:it "rest nil"           :eval "(rest nil)"}
                {:it "rest empty"         :eval "(rest [])"}]
   "next"      [{:it "next nil"           :eval "(next nil)"}
                {:it "next empty"         :eval "(next [])"}
                {:it "next single"        :eval "(next [1])"}]
   "nth"       [{:it "nth vector"         :eval "(nth [1 2 3] 1)"}
                {:it "nth default"        :eval "(nth [1 2 3] 5 :not-found)"}
                {:it "nth string"         :eval "(nth \"hello\" 1)"}]
   "count"     [{:it "count nil"          :eval "(count nil)"}
                {:it "count string"       :eval "(count \"hello\")"}
                {:it "count map"          :eval "(count {:a 1 :b 2})"}]
   "empty?"    [{:it "empty? nil"         :eval "(empty? nil)"}
                {:it "empty? \"\""        :eval "(empty? \"\")"}
                {:it "empty? []"          :eval "(empty? [])"}]
   "seq"       [{:it "seq nil"            :eval "(seq nil)"}
                {:it "seq empty"          :eval "(seq [])"}
                {:it "seq string"         :eval "(seq \"abc\")"}
                {:it "seq map"            :eval "(seq {:a 1})"}]
   "map"       [{:it "map nil"            :eval "(map inc nil)"}
                {:it "map empty"          :eval "(doall (map inc []))"}
                {:it "map multi"          :eval "(doall (map + [1 2] [3 4]))"}]
   "filter"    [{:it "filter nil"         :eval "(filter even? nil)"}
                {:it "filter none"        :eval "(doall (filter even? [1 3 5]))"}]
   "reduce"    [{:it "reduce empty"       :eval "(reduce + [])"}
                {:it "reduce init"        :eval "(reduce + 10 [1 2 3])"}
                {:it "reduce short"       :eval "(reduce (fn [a x] (if (> a 5) (reduced a) (+ a x))) 0 (range 100))"}]
   "apply"     [{:it "apply vector"       :eval "(apply vector 1 2 [3 4])"}
                {:it "apply str"          :eval "(apply str [\\a \\b \\c])"}]
   "sort"      [{:it "sort empty"         :eval "(sort [])"}
                {:it "sort reverse"       :eval "(sort > [3 1 2])"}
                {:it "sort strings"       :eval "(sort [\"b\" \"a\" \"c\"])"}]
   "concat"    [{:it "concat nil"         :eval "(doall (concat nil [1]))"}
                {:it "concat empty"       :eval "(doall (concat [] [] []))"}]
   "flatten"   [{:it "flatten deep"       :eval "(flatten [[1 [2]] [3 [4 [5]]]])"}
                {:it "flatten nil"        :eval "(flatten nil)"}]
   "range"     [{:it "range 0"            :eval "(range 0)"}
                {:it "range step"         :eval "(range 0 10 3)"}
                {:it "range neg"          :eval "(range 5 0 -1)"}]
   "partition"  [{:it "partition pad"     :eval "(partition 3 3 [:a] (range 10))"}
                 {:it "partition step"    :eval "(partition 2 1 [1 2 3 4])"}]
   "frequencies" [{:it "frequencies"      :eval "(frequencies [1 1 2 3 3 3])"}]
   "group-by"  [{:it "group-by"           :eval "(group-by even? [1 2 3 4 5])"}]
   "zipmap"    [{:it "zipmap uneven"      :eval "(zipmap [:a :b :c] [1 2])"}]

   ;; String edge cases
   "str"       [{:it "str nil"            :eval "(str nil)"}
                {:it "str multi"          :eval "(str 1 :a \"b\" nil)"}
                {:it "str no args"        :eval "(str)"}]
   "subs"      [{:it "subs range"         :eval "(subs \"hello\" 1 3)"}]
   "name"      [{:it "name keyword"       :eval "(name :foo/bar)"}
                {:it "name string"        :eval "(name \"hello\")"}]
   "namespace" [{:it "namespace plain"    :eval "(namespace :foo)"}
                {:it "namespace qual"     :eval "(namespace :foo/bar)"}]

   ;; Function composition edge cases
   "comp"      [{:it "comp no args"       :eval "((comp) 42)"}
                {:it "comp single"        :eval "((comp inc) 1)"}
                {:it "comp multi"         :eval "((comp str inc) 1)"}]
   "partial"   [{:it "partial"            :eval "((partial + 10) 5)"}
                {:it "partial multi"      :eval "((partial map inc) [1 2 3])"}]
   "juxt"      [{:it "juxt"              :eval "((juxt first last count) [1 2 3])"}]
   "complement" [{:it "complement"       :eval "((complement even?) 3)"}]
   "fnil"      [{:it "fnil nil"           :eval "((fnil inc 0) nil)"}
                {:it "fnil value"         :eval "((fnil inc 0) 5)"}]
   "every-pred" [{:it "every-pred true"  :eval "((every-pred pos? even?) 2)"}
                 {:it "every-pred false"  :eval "((every-pred pos? even?) 3)"}]
   "some-fn"   [{:it "some-fn first"     :eval "((some-fn :a :b) {:a 1})"}
                {:it "some-fn none"       :eval "((some-fn :a :b) {:c 3})"}]
   "memoize"   [{:it "memoize"           :eval "(let [f (memoize +)] [(f 1 2) (f 1 2)])"}]
   "trampoline" [{:it "trampoline"       :eval "(trampoline (fn f [n] (if (zero? n) :done #(f (dec n)))) 5)"}]

   ;; Transient edge cases
   "transient"  [{:it "transient vec"    :eval "(persistent! (conj! (transient [1 2]) 3))"}
                 {:it "transient map"    :eval "(persistent! (assoc! (transient {:a 1}) :b 2))"}]

   ;; Metadata edge cases
   "meta"      [{:it "meta nil"           :eval "(meta nil)"}
                {:it "meta fn"            :eval "(some? (meta #'inc))"}]
   "with-meta" [{:it "with-meta"          :eval "(meta (with-meta [1 2] {:tag :x}))"}]

   ;; Error handling
   "ex-info"   [{:it "ex-info"            :eval "(ex-data (ex-info \"boom\" {:code 42}))"}
                {:it "ex-message"         :eval "(ex-message (ex-info \"boom\" {}))"}]

   ;; Lazy seq edge cases
   "take"      [{:it "take neg"           :eval "(take -1 [1 2 3])"}
                {:it "take more"          :eval "(take 10 [1 2])"}]
   "drop"      [{:it "drop neg"           :eval "(drop -1 [1 2 3])"}
                {:it "drop more"          :eval "(drop 10 [1 2])"}]
   "repeat"    [{:it "repeat n"           :eval "(repeat 3 :x)"}]
   "iterate"   [{:it "iterate"            :eval "(take 5 (iterate #(* 2 %) 1))"}]
   "cycle"     [{:it "cycle"              :eval "(take 6 (cycle [1 2 3]))"}]

   ;; Destructuring / let
   "let"       [{:it "let sequential"     :eval "(let [[a b & r] [1 2 3 4]] [a b r])"}
                {:it "let map"            :eval "(let [{:keys [a b]} {:a 1 :b 2}] [a b])"}
                {:it "let defaults"       :eval "(let [{:keys [a] :or {a 42}} {}] a)"}]

   ;; Threading
   "->"        [{:it "-> chain"           :eval "(-> 1 inc (* 2) str)"}]
   "->>"       [{:it "->> chain"          :eval "(->> (range 10) (filter even?) (map inc) vec)"}]
   "as->"      [{:it "as-> mixed"         :eval "(as-> [1 2 3] x (conj x 4) (count x))"}]
   "cond->"    [{:it "cond-> true"        :eval "(cond-> 1 true inc false (* 10))"}]
   "some->"    [{:it "some-> nil"         :eval "(some-> nil inc)"}
                {:it "some-> val"         :eval "(some-> {:a 1} :a inc)"}]
   "some->>"   [{:it "some->> nil"        :eval "(some->> nil (map inc))"}]})

;; =============================================================================
;; Host interop tests (JVM-specific)
;; =============================================================================

(def host-interop-tests
  [{:category :java-constructors
    :tests
    [{:it "new Object"              :eval "(some? (Object.))"}
     {:it "new String"              :eval "(String. \"hello\")"}
     {:it "new ArrayList"           :eval "(str (java.util.ArrayList. [1 2 3]))"}
     {:it "new HashMap"             :eval "(.size (java.util.HashMap. {:a 1 :b 2}))"}]}

   {:category :java-instance-methods
    :tests
    [{:it ".length"                 :eval "(.length \"hello\")"}
     {:it ".toUpperCase"            :eval "(.toUpperCase \"hello\")"}
     {:it ".charAt"                 :eval "(.charAt \"hello\" 0)"}
     {:it ".contains"               :eval "(.contains \"hello\" \"ell\")"}
     {:it ".substring"              :eval "(.substring \"hello\" 1 4)"}
     {:it ".toString"               :eval "(.toString 42)"}]}

   {:category :java-static-methods
    :tests
    [{:it "Math/abs"                :eval "(Math/abs -42)"}
     {:it "Math/max"                :eval "(Math/max 3 7)"}
     {:it "Math/min"                :eval "(Math/min 3 7)"}
     {:it "Math/pow"                :eval "(long (Math/pow 2 10))"}
     {:it "Math/sqrt"               :eval "(Math/sqrt 4.0)"}
     {:it "Integer/parseInt"        :eval "(Integer/parseInt \"42\")"}
     {:it "Long/parseLong"          :eval "(Long/parseLong \"1000000\")"}
     {:it "String/valueOf"          :eval "(String/valueOf 42)"}
     {:it "String/format"           :eval "(String/format \"%s=%d\" (into-array Object [\"x\" 1]))"}]}

   {:category :java-static-fields
    :tests
    [{:it "Integer/MAX_VALUE"       :eval "Integer/MAX_VALUE"}
     {:it "Long/MAX_VALUE"          :eval "Long/MAX_VALUE"}
     {:it "Double/NaN"              :eval "(Double/isNaN Double/NaN)"}
     {:it "Math/PI"                 :eval "(< (abs (- Math/PI 3.14159)) 0.001)"}
     {:it "Boolean/TRUE"            :eval "Boolean/TRUE"}]}

   {:category :java-interop-forms
    :tests
    [{:it "doto"                    :eval "(str (doto (java.util.ArrayList.) (.add 1) (.add 2)))"}
     {:it ".. chain"                :eval "(.. \"hello\" toUpperCase (substring 0 3))"}
     {:it "instance? String"        :eval "(instance? String \"hello\")"}
     {:it "instance? Long"          :eval "(instance? Long 42)"}
     {:it "class of string"         :eval "(str (class \"hello\"))"}
     {:it "class of int"            :eval "(str (class 42))"}
     {:it "bean"                    :eval "(contains? (bean (java.util.Date.)) :time)"}]}

   {:category :java-collections-interop
    :tests
    [{:it "java list to vec"        :eval "(vec (java.util.Arrays/asList (into-array [1 2 3])))"}
     {:it "iterate java map"        :eval "(count (java.util.HashMap. {:a 1 :b 2 :c 3}))"}
     {:it "System/getenv"           :eval "(string? (or (System/getenv \"PATH\") \"\"))"}
     {:it "System/currentTimeMillis" :eval "(> (System/currentTimeMillis) 0)"}]}

   {:category :java-type-identity
    :parametric
    [{:describe "type identity"
      :params {:val ["42" "3.14" "\"hello\"" "true" "\\a" ":a" "[]" "{}" "'()" "#{}"]
               }
      :template "(str (class %val))"}]}])

;; =============================================================================
;; Scaling tests (complexity verification)
;; =============================================================================

(def scaling-tests
  [{:category :scaling-O1
    :tests
    [{:it "vector-nth-O1"
      :eval "(let [v (vec (range 100000))] (nth v (quot (count v) 2)))"
      :scaling true :sizes [100 1000 10000 100000] :expected-complexity :O1}
     {:it "vector-conj-O1"
      :eval "(let [v (vec (range 100000))] (conj v :x))"
      :scaling true :sizes [1000 10000 100000 1000000] :expected-complexity :O1}
     {:it "map-get-O1"
      :eval "(let [m (into {} (map (fn [i] [i i]) (range 100000)))] (get m (quot (count m) 2)))"
      :scaling true :sizes [100 1000 10000 100000] :expected-complexity :O1}
     {:it "map-assoc-O1"
      :eval "(let [m (into {} (map (fn [i] [i i]) (range 100000)))] (assoc m :new :value))"
      :scaling true :sizes [100 1000 10000 100000] :expected-complexity :O1}
     {:it "set-contains-O1"
      :eval "(let [s (set (range 100000))] (contains? s (quot (count s) 2)))"
      :scaling true :sizes [100 1000 10000 100000] :expected-complexity :O1}]}])

;; =============================================================================
;; Arg name → pool heuristics
;; =============================================================================

(defn arg-pool
  "Pick a value pool for an argument based on its name and namespace context."
  [arg-name ns-name]
  (let [s (str arg-name)
        p (pools)]
    (cond
      ;; Exact matches
      (= s "&")               nil  ;; skip rest marker
      (#{"n" "num" "x" "y"
         "a" "b" "start" "end"
         "step" "init" "from"
         "to" "dividend" "divisor"
         "index" "i" "j"} s)            (:num p)
      (#{"s" "string" "cs"
         "substr" "replacement"} s)     (:string p)
      (#{"coll" "c" "c1" "c2"
         "c3" "colls"} s)               (:coll p)
      (#{"f" "fn" "pred" "g"} s)        (:fn p)
      (#{"xform" "xf"} s)               (:xf p)
      (#{"k" "key"} s)                  (:keyword p)
      (#{"ks" "keys"} s)                (:keyword p)
      (#{"v" "val" "e"} s)              (:any p)
      (#{"m" "map" "kmap" "smap"} s)    (:map p)
      (#{"re" "pattern" "match"} s)     (:regex p)
      (#{"t" "tag"} s)                  (:keyword p)
      (#{"xrel" "yrel" "xset"
         "s1" "s2" "set1" "set2"} s)    (:set p)
      (#{"ch" "c"} s)                   (:char p)

      ;; Pattern matches
      (str/ends-with? s "map")          (:map p)
      (str/ends-with? s "set")          (:set p)
      (str/ends-with? s "?")            (:any p)

      ;; Namespace context defaults
      (= ns-name "clojure.string")      (:string p)
      (= ns-name "clojure.math")        (:num p)
      (= ns-name "clojure.set")         (:set p)

      ;; Fallback
      :else                              (:any p))))

(defn limit-pool
  "Limit pool size based on arity and tier."
  [pool arity]
  (let [limits {:quick    {1 1, 2 1, 3 1, 4 1}
                :balanced {1 4, 2 3, 3 2, 4 2}
                :thorough {1 8, 2 5, 3 3, 4 2}}
        tier-limits (get limits *tier* (:balanced limits))
        max-per-arg (get tier-limits (min arity 4) 1)]
    (vec (take max-per-arg pool))))

;; =============================================================================
;; Function classification
;; =============================================================================

(def skip-reasons
  "Vars that need hand-written tests, grouped by reason."
  {:dynamic-vars
   {:reason "Dynamic vars — test via binding forms"
    :vars #{'*ns* '*out* '*err* '*in* '*file* '*command-line-args* '*print-length*
            '*print-level* '*print-meta* '*print-dup* '*print-readably* '*flush-on-newline*
            '*read-eval* '*data-readers* '*default-data-reader-fn* '*assert*
            '*math-context* '*agent* '*1 '*2 '*3 '*e '*warn-on-reflection*
            '*unchecked-math* '*compiler-options* '*compile-path* '*compile-files*
            '*allow-unresolved-vars* '*reader-resolver* '*source-path* '*use-context-classloader*
            '*verbose-defrecords* '*fn-loader* '*suppress-read*}}

   :side-effects
   {:reason "Side effects — need isolation or are destructive"
    :vars #{'shutdown-agents 'alter-var-root 'intern
            'ns-unmap 'ns-unalias 'remove-ns 'in-ns
            'load 'load-file 'load-reader 'load-string
            'require 'use 'import 'refer 'refer-clojure
            'compile 'gen-class 'gen-interface}}

   :agents
   {:reason "Agent ops — need agent context + async"
    :vars #{'send 'send-off 'send-via 'restart-agent
            'set-agent-send-executor! 'set-agent-send-off-executor!
            'await 'await-for 'await1
            'add-watch 'remove-watch 'set-validator!}}

   :refs
   {:reason "Ref/STM ops — need (dosync) + ref context"
    :vars #{'dosync 'commute 'alter 'ref-set 'ensure}}

   :atoms
   {:reason "Atom ops — need (atom) context"
    :vars #{'swap! 'reset! 'swap-vals! 'reset-vals!}}

   :volatiles
   {:reason "Volatile ops — need (volatile!) context"
    :vars #{'vswap! 'vreset!}}

   :promises
   {:reason "Promise ops — need (promise) context"
    :vars #{'deliver}}

   :async
   {:reason "Async/threading — test separately with timeouts"
    :vars #{'future 'future-call 'pmap 'pcalls 'pvalues 'locking 'io!}}

   :io
   {:reason "I/O — need streams, files, or readers"
    :vars #{'slurp 'spit 'print 'println 'pr 'prn 'printf 'newline 'flush
            'read 'read-line 'read-string}}

   :hierarchy
   {:reason "Hierarchy ops — need (make-hierarchy) context"
    :vars #{'class 'type 'supers 'bases 'parents 'ancestors 'descendants
            'make-hierarchy 'isa? 'derive 'underive}}

   :eval
   {:reason "Eval/macroexpand — need controlled expressions"
    :vars #{'eval 'macroexpand 'macroexpand-1 'special-symbol? 'find-keyword}}

   :binding-forms
   {:reason "Binding macros — need var + value setup"
    :vars #{'binding 'with-bindings 'with-bindings*
            'with-redefs 'with-redefs-fn}}})

(def skip-vars
  "Flat set of all skip vars."
  (into #{} (mapcat (comp :vars val)) skip-reasons))

(defn skip-reason
  "Return the skip-reason key for a var, or nil if not skipped."
  [sym ns-name]
  (let [lookup-sym (if (= ns-name "clojure.core") sym
                       (symbol (str ns-name "/" (name sym))))]
    (some (fn [[reason-key {:keys [vars]}]]
            (when (contains? vars lookup-sym) reason-key))
          skip-reasons)))

(defn classify-var [sym var-meta ns-name]
  (let [nm (name sym)
        macro? (:macro var-meta)
        arglists (:arglists var-meta)
        sr (skip-reason sym ns-name)
        min-arity (when arglists
                    (apply min (map #(count (take-while (fn [a] (not= '& a)) %)) arglists)))
        unary-pred? (and (str/ends-with? nm "?") (= 1 min-arity))]
    (cond
      sr                                  [:skip sr]
      (nil? arglists)                     [:skip :not-a-fn]
      (str/starts-with? nm "->")          [:skip :constructor]
      (str/starts-with? nm "map->")       [:skip :constructor]
      (> (or min-arity 0) 4)             [:skip :high-arity]
      macro?                              :macro
      unary-pred?                         :predicate
      :else                               :function)))

;; =============================================================================
;; Spec generation
;; =============================================================================

(defn arg-name
  "Extract a usable name from an arglist entry (handles destructuring)."
  [arg]
  (cond
    (symbol? arg) (name arg)
    (vector? arg) "v"     ;; vector destructuring
    (map? arg)    "m"     ;; map destructuring
    :else         "x"))

(defn gen-parametric
  "Generate a parametric spec entry for a function var."
  [sym var-meta ns-name]
  (let [arglists (:arglists var-meta)
        ;; Pick the most useful arity (shortest non-zero, or zero if that's all there is)
        sorted (sort-by count arglists)
        target-arity (or (first (filter #(pos? (count %)) sorted))
                         (first sorted))
        ;; Remove & and rest args for parametric
        fixed-args (take-while #(not= '& %) target-arity)
        arity (count fixed-args)]
    (when (and (pos? arity) (<= arity 4))
      (let [;; Deduplicate arg names (some fns have [x x] style via destructuring)
            named-args (map-indexed
                         (fn [i arg]
                           (let [base (arg-name arg)]
                             (if (> (count (filter #(= base (arg-name %)) fixed-args)) 1)
                               (str base (inc i))
                               base)))
                         fixed-args)
            params (into {}
                     (map (fn [arg orig]
                            [(keyword arg)
                             (limit-pool (arg-pool orig ns-name) arity)])
                          named-args fixed-args))
            qualified (if (= ns-name "clojure.core")
                        (name sym)
                        (str ns-name "/" (name sym)))
            template (str "(" qualified
                          (str/join "" (map #(str " %" %) named-args))
                          ")")]
        {:describe (name sym)
         :params params
         :template template}))))

(defn gen-nullary
  "Generate test entries for zero-arity functions."
  [sym var-meta ns-name]
  (try
    (let [arglists (:arglists var-meta)
          has-nullary? (some #(zero? (count %)) arglists)
          qualified (if (= ns-name "clojure.core")
                      (name sym)
                      (str ns-name "/" (name sym)))]
      (when has-nullary?
        {:it (name sym) :eval (str "(" qualified ")")}))
    (catch Exception _ nil)))

(defn gen-predicate-spec
  "For predicates, test against ALL value types."
  [sym var-meta ns-name]
  (let [qualified (if (= ns-name "clojure.core")
                    (name sym)
                    (str ns-name "/" (name sym)))]
    {:describe (name sym)
     :params {:val (:any (pools))}
     :template (str "(" qualified " %val)")}))

(defn get-edge-cases
  "Get edge case tests for a var, if any exist."
  [sym ns-name]
  (let [qualified (if (= ns-name "clojure.core")
                    (name sym)
                    (str ns-name "/" (name sym)))]
    (get edge-cases qualified)))

;; =============================================================================
;; Namespace processing
;; =============================================================================

(defn process-namespace
  "Reflect on a namespace, generate spec entries."
  [ns-name]
  (try
    (require (symbol ns-name))
    (let [ns-sym (symbol ns-name)
          publics (sort-by key (ns-publics ns-sym))
          ;; Classify all vars
          classified (for [[sym v] publics
                           :let [m (meta v)
                                 cls (classify-var sym m ns-name)]]
                       {:sym sym :vmeta m :class cls})
          ;; Collect skips with reasons
          skipped-vars (for [{:keys [sym class]} classified
                             :when (and (vector? class) (= :skip (first class)))]
                         {:var (name sym) :reason (second class)})
          ;; Generate specs for non-skipped
          active (remove #(and (vector? (:class %)) (= :skip (first (:class %)))) classified)
          entries (for [{:keys [sym vmeta class]} active]
                    (case class
                      :predicate  {:type :parametric :entry (gen-predicate-spec sym vmeta ns-name)}
                      :function   {:type :parametric :entry (gen-parametric sym vmeta ns-name)}
                      :macro      {:type :parametric :entry (gen-parametric sym vmeta ns-name)}
                      nil))
          parametrics (vec (keep #(when (= (:type %) :parametric) (:entry %)) entries))
          ;; Nullary tests
          nullaries (vec (keep (fn [{:keys [sym vmeta]}]
                                 (gen-nullary sym vmeta ns-name))
                               active))
          ;; Edge case tests
          edge-case-tests (vec (mapcat (fn [{:keys [sym]}]
                                         (get-edge-cases sym ns-name))
                                       active))
          ;; All explicit tests: nullary + edge cases
          all-tests (vec (concat nullaries edge-case-tests))
          generated (count (filter some? parametrics))]
      {:ns ns-name
       :total (count publics)
       :generated generated
       :skipped (count skipped-vars)
       :skipped-vars (vec skipped-vars)
       :parametric (vec (filter some? parametrics))
       :tests all-tests})
    (catch Exception e
      {:ns ns-name :error (str e) :total 0 :generated 0 :skipped 0
       :skipped-vars [] :parametric [] :tests []})))


(defn ->spec-edn
  "Convert processed namespace to parity-compatible spec format.
   Uses semantic categories for clojure.core, flat category for others."
  [{:keys [ns parametric tests]}]
  (if (= ns "clojure.core")
    ;; Group core vars by semantic category
    (let [categorized (group-by (fn [entry]
                                  (let [sym (symbol (or (:describe entry) (:it entry) ""))]
                                    (get var->category sym :uncategorized)))
                                (concat parametric tests))
          specs (for [[cat entries] (sort-by key categorized)
                      :when (seq entries)]
                  (let [params (filter :params entries)
                        explicit (remove :params entries)]
                    (cond-> {:category cat}
                      (seq explicit) (assoc :tests (vec explicit))
                      (seq params)   (assoc :parametric (vec params)))))]
      specs)
    ;; Other namespaces: single category
    (let [cat (keyword (str (str/replace ns "." ".") ".functions"))]
      [(cond-> {:category cat}
         (seq tests)      (assoc :tests (vec tests))
         (seq parametric) (assoc :parametric (vec parametric)))])))

;; =============================================================================
;; Output
;; =============================================================================

(defn print-stats [results]
  (println "=== Spec Generation Stats ===\n")
  (let [total-gen (reduce + (map :generated results))
        total-all (reduce + (map :total results))
        all-skipped (mapcat :skipped-vars results)]
    (doseq [{:keys [ns total generated skipped error]} results]
      (if error
        (println (format "  %-35s ERROR: %s" ns error))
        (println (format "  %-35s %4d public  %4d generated  %4d skipped"
                         ns total generated skipped))))
    (println (format "\n  Total: %d vars, %d test groups generated" total-all total-gen))

    ;; Skip report — grouped by reason
    (when (seq all-skipped)
      (let [by-reason (group-by :reason all-skipped)]
        (println (format "\n=== NEED HAND-WRITTEN TESTS (%d vars) ===\n" (count all-skipped)))
        (doseq [[reason-key {:keys [reason]}] (sort-by key skip-reasons)
                :let [vars (get by-reason reason-key)]
                :when (seq vars)]
          (println (format "  %s (%d):" reason (count vars)))
          (println (format "    %s" (str/join ", " (sort (map :var vars)))))
          (println))
        ;; Also show structural skips (not-a-fn, constructor, high-arity)
        (doseq [reason-key [:not-a-fn :constructor :high-arity]
                :let [vars (get by-reason reason-key)]
                :when (seq vars)]
          (let [label (case reason-key
                        :not-a-fn    "Not a function (constants, protocols)"
                        :constructor "Record constructors (->Type, map->Type)"
                        :high-arity  "High arity (>4 args) — reduce manually")]
            (println (format "  %s (%d):" label (count vars)))
            (println (format "    %s" (str/join ", " (sort (map :var vars)))))
            (println)))))))

;; =============================================================================
;; Namespace lists (needed by write-specs for lang/contrib split)
;; =============================================================================

(def default-namespaces
  "All namespaces that ship with Clojure + spec."
  [;; Core
   "clojure.core"
   "clojure.core.protocols"
   "clojure.core.reducers"
   "clojure.core.server"
   ;; Data
   "clojure.data"
   "clojure.datafy"
   "clojure.edn"
   ;; Standard library
   "clojure.instant"
   "clojure.math"
   "clojure.pprint"
   "clojure.repl"
   "clojure.set"
   "clojure.stacktrace"
   "clojure.string"
   "clojure.template"
   "clojure.test"
   "clojure.walk"
   "clojure.xml"
   "clojure.zip"
   ;; Java interop
   "clojure.java.browse"
   "clojure.java.io"
   "clojure.java.javadoc"
   "clojure.java.process"
   "clojure.java.shell"
   ;; Reflection
   "clojure.main"
   "clojure.reflect"
   ;; Spec (ships separately but is Cognitect)
   "clojure.spec.alpha"
   "clojure.spec.gen.alpha"
   "clojure.spec.test.alpha"])

(def contrib-namespaces
  "Contrib libraries (from deps.edn)."
  [;; Active Cognitect
   "clojure.core.async"
   "clojure.core.cache" "clojure.core.memoize"
   "clojure.data.csv" "clojure.data.json" "clojure.data.xml"
   "clojure.data.zip" "clojure.data.priority-map"
   "clojure.data.int-map" "clojure.data.avl"
   "clojure.java.data" "clojure.java.classpath"
   "clojure.math.combinatorics" "clojure.math.numeric-tower"
   "clojure.test.check" "clojure.test.check.generators"
   "clojure.tools.cli" "clojure.tools.logging"
   "clojure.tools.namespace.find"
   "clojure.tools.reader" "clojure.tools.reader.edn"
   "clojure.tools.trace"
   ;; Stable
   "clojure.core.logic" "clojure.core.match"
   "clojure.core.unify"])

(def all-namespaces
  "All namespaces: shipped + contrib."
  (vec (concat default-namespaces contrib-namespaces)))

(defn print-spec [results]
  (println "[")
  (doseq [r results :when (or (seq (:tests r)) (seq (:parametric r)))]
    (let [specs (->spec-edn r)]
      (doseq [spec specs]
        (clojure.pprint/pprint spec)
        (println))))
  (println "]"))

(defn write-specs
  "Write spec files. For clojure.core, produces multiple category files.
   lang-dir is for shipped namespaces, contrib-dir for contrib."
  [results lang-dir contrib-dir]
  (doseq [r results :when (or (seq (:tests r)) (seq (:parametric r)))]
    (let [specs (->spec-edn r)
          is-contrib? (contains? (set contrib-namespaces) (:ns r))
          dir (if is-contrib? contrib-dir lang-dir)]
      (if (= (:ns r) "clojure.core")
        ;; Core: single file with all categories
        (let [path (str dir "/clojure.core.edn")]
          (io/make-parents path)
          (spit path (with-out-str
                       (println "[")
                       (doseq [spec specs]
                         (clojure.pprint/pprint spec)
                         (println))
                       (println "]")))
          (println (format "  wrote %s (%d categories)" path (count specs))))
        ;; Other: one file per namespace
        (let [fname (str (str/replace (:ns r) "." ".") ".edn")
              path (str dir "/" fname)
              spec (first specs)]
          (io/make-parents path)
          (spit path (with-out-str
                       (println "[")
                       (clojure.pprint/pprint spec)
                       (println "]")))
          (println (format "  wrote %s (%d groups)"
                           path (+ (count (:tests r)) (count (:parametric r))))))))))

(defn write-host-specs
  "Write host interop and scaling test files."
  [lang-dir]
  (let [host-path (str lang-dir "/host.edn")
        scale-path (str lang-dir "/scaling.edn")]
    (io/make-parents host-path)
    (spit host-path (with-out-str
                      (println "[")
                      (doseq [spec host-interop-tests]
                        (clojure.pprint/pprint spec)
                        (println))
                      (println "]")))
    (println (format "  wrote %s (%d categories)" host-path (count host-interop-tests)))

    (spit scale-path (with-out-str
                       (println "[")
                       (doseq [spec scaling-tests]
                         (clojure.pprint/pprint spec)
                         (println))
                       (println "]")))
    (println (format "  wrote %s (%d categories)" scale-path (count scaling-tests)))))

;; =============================================================================
;; Coverage analysis — host.cljc vs JVM host surface
;; Host data comes from depgraph (--host-edn), not regex.
;; =============================================================================

(defn extract-protocol-methods
  "Parse host.cljc and extract all protocol method names."
  [host-file]
  (let [content (slurp host-file)
        ;; Match (-method-name [h ...] "doc") patterns
        methods (re-seq #"\((-[\w?!*+-]+)\s+\[" content)]
    (vec (distinct (map second methods)))))

(defn extract-defns
  "Extract all defn/def names from a .cljc file."
  [file]
  (let [content (slurp file)
        defs (re-seq #"\(defn?-?\s+([\w?!*+-]+)" content)]
    (vec (distinct (map second defs)))))

(defn extract-host-calls
  "Find all Host protocol method calls in a .cljc file.
   Matches patterns like (-method-name (host) ...) and (host/-method ...)."
  [file]
  (let [content (slurp file)
        ;; Direct protocol calls: (-method-name (host) ...)
        direct (re-seq #"\((-[\w?!*+-]+)\s+\(host\)" content)
        ;; Also match convenience wrappers that call protocol
        wrapper (re-seq #"\((-[\w?!*+-]+)\s+\*host\*" content)]
    (vec (distinct (map second (concat direct wrapper))))))

(defn extract-jvm-refs
  "Find JVM host references in a .cljc file (reader conditionals, interop)."
  [file]
  (let [content (slurp file)
        ;; clojure.lang.* refs
        lang-refs (re-seq #"clojure\.lang\.\w+" content)
        ;; java.* refs
        java-refs (re-seq #"java\.\w+\.\w+" content)
        ;; .method calls
        methods (re-seq #"\(\.\w+" content)
        ;; Class/method statics
        statics (re-seq #"[A-Z]\w+/\w+" content)]
    {:clojure-lang (vec (distinct lang-refs))
     :java (vec (distinct java-refs))
     :methods (vec (distinct methods))
     :statics (vec (distinct statics))}))

(defn analyze-ported-file
  "Analyze a single ported .cljc file."
  [file]
  (let [fname (.getName (io/file file))]
    {:file fname
     :path (str file)
     :ns (str/replace (str/replace fname #"\.cljc$" "") "_" "-")
     :defns (extract-defns file)
     :host-calls (extract-host-calls file)
     :jvm-refs (extract-jvm-refs file)}))

(defn do-coverage
  "Cross-reference host.cljc protocol with ported files and JVM host analysis."
  [ported-dir host-analysis]
  (let [host-file (str ported-dir "/host.cljc")
        _ (when-not (.exists (io/file host-file))
            (println (str "ERROR: " host-file " not found"))
            (System/exit 1))
        ;; 1. Extract Host protocol
        protocol-methods (extract-protocol-methods host-file)

        ;; 2. Scan all .cljc files
        cljc-files (->> (file-seq (io/file ported-dir))
                        (filter #(str/ends-with? (.getName %) ".cljc"))
                        (sort-by #(.getName %)))
        ported (mapv #(analyze-ported-file (.getPath %)) cljc-files)

        ;; 3. Aggregate
        all-ported-defns (into #{} (mapcat :defns) ported)
        all-host-calls (into #{} (mapcat :host-calls) ported)
        unused-protocol (remove (set all-host-calls) protocol-methods)
        unknown-calls (remove (set protocol-methods) all-host-calls)

        ;; 4. Cross-reference with JVM analysis
        ;; For each JVM namespace, check if ported covers the host-dependent vars
        jvm-host-vars (for [{:keys [ns vars-host]} host-analysis
                            {:keys [var]} vars-host]
                        {:ns ns :var var})
        jvm-pure-vars (for [{:keys [ns vars-pure]} host-analysis
                            var vars-pure]
                        {:ns ns :var var})

        ;; Map JVM namespace → ported .cljc file(s)
        ns-to-files {"clojure.core" ["pcore.cljc" "pmcore.cljc"]
                      "clojure.string" ["string.cljc"]
                      "clojure.set" ["set.cljc"]
                      "clojure.walk" ["walk.cljc"]
                      "clojure.data" ["data.cljc"]
                      "clojure.zip" ["zip.cljc"]
                      "clojure.math" ["math.cljc"]
                      "clojure.xml" ["xml.cljc"]
                      "clojure.pprint" ["pprint.cljc"]
                      "clojure.edn" ["edn.cljc"]
                      "clojure.test" ["test.cljc"]
                      "clojure.repl" ["repl.cljc"]
                      "clojure.template" ["template.cljc"]
                      "clojure.stacktrace" ["stacktrace.cljc"]
                      "clojure.datafy" ["datafy.cljc"]
                      "clojure.main" ["main.cljc"]
                      "clojure.java.io" ["io.cljc"]
                      "clojure.java.shell" ["shell.cljc"]
                      "clojure.java.process" ["process.cljc"]
                      "clojure.java.browse" ["browse.cljc"]
                      "clojure.core.async" ["async.cljc"]
                      "clojure.core.protocols" ["protocols.cljc"]
                      "clojure.data.csv" ["csv.cljc"]
                      "clojure.data.json" ["json.cljc"]}
        ported-by-file (into {} (map (fn [p] [(:file p) p])) ported)
        ported-file-set (set (map :file ported))
        ;; Unmapped ported files (extras beyond what JVM namespaces map to)
        mapped-files (into #{} (mapcat val) ns-to-files)
        extra-files (remove mapped-files (map :file ported))]

    ;; === Output ===
    (println "=== HOST COVERAGE ANALYSIS ===")
    (println (format "Host protocol: %s (%d methods)" host-file (count protocol-methods)))
    (println (format "Ported files:  %d .cljc files, %d total defns"
                     (count ported) (count all-ported-defns)))
    (println)

    ;; Protocol method usage
    (println (format "--- HOST PROTOCOL METHODS (%d) ---\n" (count protocol-methods)))
    (println (format "  Used:   %d" (count all-host-calls)))
    (println (format "  Unused: %d" (count unused-protocol)))
    (when (seq unused-protocol)
      (println "  Unused methods:")
      (doseq [m (sort unused-protocol)]
        (println (format "    %s" m))))
    (when (seq unknown-calls)
      (println (format "\n  Unknown calls (not in protocol): %d" (count unknown-calls)))
      (doseq [m (sort unknown-calls)]
        (println (format "    %s" m))))

    ;; Per-file host call breakdown
    (println (format "\n--- HOST CALLS PER FILE ---\n"))
    (doseq [{:keys [file defns host-calls jvm-refs]} (sort-by :file ported)
            :when (or (seq host-calls) (seq (:clojure-lang jvm-refs)))]
      (println (format "  %-25s %3d defns  %2d host calls  %2d JVM refs"
                       file (count defns) (count host-calls)
                       (+ (count (:clojure-lang jvm-refs))
                          (count (:java jvm-refs))))))

    ;; Namespace coverage vs JVM
    (println (format "\n--- NAMESPACE COVERAGE (JVM → ported) ---\n"))
    (println (format "  %-40s %5s %5s %5s %5s  %s" "NAMESPACE" "JVM" "PORT" "HOST?" "COVER" "FILE(S)"))
    (println (str "  " (apply str (repeat 100 "-"))))
    (doseq [{:keys [ns total pure host]} (sort-by :ns host-analysis)
            :when (pos? total)]
      (let [files (get ns-to-files ns)
            port-files (when files (keep ported-by-file files))
            has-port? (seq port-files)
            port-defns (reduce + 0 (map #(count (:defns %)) port-files))
            port-jvm-refs (reduce + 0 (map (fn [p]
                                              (let [jr (:jvm-refs p)]
                                                (+ (count (:clojure-lang jr))
                                                   (count (:java jr)))))
                                            port-files))
            pct (if (and has-port? (pos? total))
                  (format "%d%%" (min 100 (int (* 100.0 (/ (double port-defns) total)))))
                  "—")
            file-str (if files (str/join " + " files) "NOT MAPPED")]
        (println (format "  %-40s %5d %5s %5s %5s  %s"
                         ns total
                         (if has-port? (str port-defns) "—")
                         (if has-port? (str port-jvm-refs) "—")
                         pct
                         file-str))))

    ;; Missing namespaces (JVM has them, no ported file)
    (let [missing (for [{:keys [ns total]} host-analysis
                        :when (and (pos? total)
                                   (not (get ns-to-files ns)))]
                    ns)]
      (when (seq missing)
        (println (format "\n--- NOT YET PORTED (%d namespaces) ---\n" (count missing)))
        (doseq [ns-name (sort missing)]
          (let [info (first (filter #(= (:ns %) ns-name) host-analysis))]
            (println (format "  %-40s %3d vars (%d pure, %d host)"
                             ns-name (:total info) (:pure info) (:host info)))))))

    ;; Extra ported files (not mapped to any JVM namespace)
    (when (seq extra-files)
      (println (format "\n--- EXTRA PORTED FILES (no JVM namespace mapping) ---\n"))
      (doseq [f (sort extra-files)]
        (let [p (get ported-by-file f)
              jr (:jvm-refs p)]
          (println (format "  %-25s %3d defns  %2d host-calls  %2d JVM refs"
                           f (count (:defns p)) (count (:host-calls p))
                           (+ (count (:clojure-lang jr)) (count (:java jr))))))))

    ;; Residual JVM refs in portable files (should be zero for true portability)
    (let [portable-with-refs (for [p ported
                                   :let [jr (:jvm-refs p)
                                         n (+ (count (:clojure-lang jr))
                                              (count (:java jr))
                                              (count (:methods jr))
                                              (count (:statics jr)))]
                                   :when (pos? n)
                                   ;; Skip core.cljc (upstream reference, not portable)
                                   :when (not= (:file p) "core.cljc")]
                               (assoc p :total-jvm-refs n))]
      (when (seq portable-with-refs)
        (println (format "\n--- RESIDUAL JVM REFS IN PORTED FILES (%d files) ---\n"
                         (count portable-with-refs)))
        (doseq [p (sort-by :total-jvm-refs > portable-with-refs)]
          (let [jr (:jvm-refs p)]
            (println (format "  %-25s %3d refs" (:file p) (:total-jvm-refs p)))
            (when (seq (:clojure-lang jr))
              (println (format "    clojure.lang: %s" (str/join ", " (:clojure-lang jr)))))
            (when (seq (:java jr))
              (println (format "    java.*:       %s" (str/join ", " (:java jr)))))
            (when (seq (:statics jr))
              (println (format "    statics:      %s" (str/join ", " (take 10 (:statics jr))))))))))

    ;; Summary
    (let [mapped-ns (count (filter #(get ns-to-files (:ns %)) host-analysis))
          total-ns (count (filter #(pos? (:total %)) host-analysis))
          total-jvm-vars (reduce + (map :total host-analysis))
          total-ported (count all-ported-defns)]
      (println "\n--- SUMMARY ---\n")
      (println (format "  JVM namespaces analyzed:  %d" total-ns))
      (println (format "  Mapped to .cljc files:    %d" mapped-ns))
      (println (format "  JVM public vars:          %d" total-jvm-vars))
      (println (format "  Ported definitions:       %d" total-ported))
      (println (format "  Host protocol methods:    %d (%d used)" (count protocol-methods) (count all-host-calls))))))

;; =============================================================================
;; Main
;; =============================================================================

(defn -main [& args]
  (let [args (vec args)
        tier (cond
               (some #{"--quick"} args)    :quick
               (some #{"--thorough"} args) :thorough
               :else                       :balanced)
        args (vec (remove #{"--quick" "--balanced" "--thorough"} args))
        pairs (partition 2 1 args)
        write-dir    (some #(when (= "--write" (first %)) (second %)) pairs)
        ;; --write takes two args: lang-dir contrib-dir
        write-args   (when write-dir
                       (let [idx (.indexOf args "--write")]
                         (when (< (+ idx 2) (count args))
                           [(nth args (+ idx 1)) (nth args (+ idx 2))])))
        lang-dir     (first write-args)
        contrib-dir  (second write-args)
        coverage-dir (some #(when (= "--coverage" (first %)) (second %)) pairs)
        host-data    (some #(when (= "--host-data" (first %)) (second %)) pairs)
        stats-only   (some #{"--stats"} args)
        ns-args (remove #(str/starts-with? % "--") args)
        ns-args (reduce (fn [a dir] (if dir (remove #{dir} a) a))
                         ns-args [lang-dir contrib-dir coverage-dir host-data])
        namespaces (if (seq ns-args) (vec ns-args) all-namespaces)]
    (binding [*tier* tier]
    (cond
      coverage-dir
      (let [_ (when-not host-data
                (println "ERROR: --coverage requires --host-data <file.edn>")
                (println "  Generate with: par deps <clojure-src> --host-edn > host.edn")
                (System/exit 1))
            host-results (edn/read-string (slurp host-data))
            _ (binding [*out* *err*]
                (println (format "specgen: loaded %d namespaces from %s" (count host-results) host-data)))]
        (do-coverage coverage-dir host-results))

      :else
      (let [_ (binding [*out* *err*]
                (println (format "specgen: reflecting on %d namespaces..." (count namespaces))))
            results (mapv process-namespace namespaces)]
        (cond
          stats-only  (print-stats results)
          (and lang-dir contrib-dir)
          (do (print-stats results)
              (println (format "\nWriting lang specs to %s, contrib to %s:" lang-dir contrib-dir))
              (write-specs results lang-dir contrib-dir)
              (write-host-specs lang-dir))
          :else       (do (print-stats results)
                          (println "\n--- Generated Specs ---\n")
                          (print-spec results))))))))

(apply -main *command-line-args*)
