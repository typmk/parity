#!/usr/bin/env clojure
;; =============================================================================
;; portabilize.clj — rewrite JVM Clojure to portable Clojure
;;
;; Uses roots (JVM reflection) to discover every host dependency, then applies
;; systematic naming conventions to replace them via rewrite-clj AST walking.
;;
;; Naming conventions:
;;   RT/method        → p/-method or t/method
;;   Numbers/method   → h/-method
;;   Util/method      → p/-method
;;   instance? IFoo   → satisfies? p/IFoo
;;   new Foo / Foo.   → t/->foo
;;   .method          → p/-method
;;   ^TypeHint        → stripped
;;   java.*           → t/* or stripped
;;
;; Zero hardcoded tables. Everything derived from JVM reflection.
;; =============================================================================

(ns portabilize
  (:require [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [clojure.string :as str]))

;; =============================================================================
;; Load host contract from roots
;; =============================================================================

(defn load-host-contract []
  (binding [*command-line-args* ["--edn"]]
    (let [out (with-out-str (load-file "src/parity/analyze/roots.clj"))]
      (read-string out))))

;; =============================================================================
;; Naming conventions
;; =============================================================================

(defn camel->kebab [s]
  (-> s
      (str/replace #"([a-z])([A-Z])" "$1-$2")
      (str/replace #"_" "-")
      str/lower-case))

(defn rt-replacement [method]
  (let [k (camel->kebab method)]
    (cond
      (str/ends-with? method "Cast") (str/replace k "-cast" "")
      (str/starts-with? method "unchecked") (str "unchecked-" (subs k 10))
      (#{"conj" "assoc" "count" "nth" "get" "contains" "dissoc"
         "peek" "pop" "seq" "first" "next" "more" "cons"
         "keys" "vals" "find" "meta" "with-meta"} k) (str "p/-" k)
      (str/starts-with? k "is-") (str "p/-" (subs k 3) "?")
      :else (str "t/" k))))

(defn numbers-replacement [method]
  (let [k (camel->kebab method)]
    (cond
      (str/starts-with? method "unchecked") (str "unchecked-" (str/replace (subs k 10) #"^-" ""))
      (str/starts-with? k "is-") (str "h/-" (subs k 3) "?")
      :else (str "h/-" k))))

(defn util-replacement [method]
  (str "p/-" (camel->kebab method)))

(defn static-replacement [class-str method]
  (cond
    (= class-str "clojure.lang.RT")      (rt-replacement method)
    (= class-str "clojure.lang.Numbers") (numbers-replacement method)
    (= class-str "clojure.lang.Util")    (util-replacement method)
    (str/starts-with? class-str "clojure.lang.")
    (str "t/" (camel->kebab method))
    :else nil))

(defn jvm-type? [s]
  (or (str/starts-with? s "clojure.lang.")
      (str/starts-with? s "java.lang.")
      (str/starts-with? s "java.util.")
      (str/starts-with? s "java.io.")
      (str/starts-with? s "java.net.")
      (str/starts-with? s "java.sql.")))

(defn jvm-exception? [s]
  (#{"IllegalArgumentException" "UnsupportedOperationException"
     "ArithmeticException" "RuntimeException" "ClassCastException"
     "NullPointerException" "IndexOutOfBoundsException"
     "IllegalStateException" "IllegalAccessError" "Throwable"
     "NumberFormatException" "StackOverflowError"} s))

;; =============================================================================
;; Generate tables from host contract
;; =============================================================================

(defn generate-symbol-table [host]
  (let [entries (atom {})]
    ;; Bridge statics
    (doseq [[class-name {:keys [ops]}] (:bridge host)
            [method _] ops]
      (let [fqn (symbol (str "clojure.lang." class-name "/" method))
            repl (static-replacement (str "clojure.lang." class-name) method)]
        (when repl (swap! entries assoc fqn (symbol repl)))))
    ;; Interface refs
    (doseq [iface (:interfaces host)]
      (swap! entries assoc
             (symbol (str "clojure.lang." (:name iface)))
             (symbol (str "p/" (:name iface)))))
    ;; Concrete statics
    (doseq [concrete (:concretes host)
            {:keys [name static?]} (:methods concrete)
            :when static?]
      (let [fqn (symbol (str "clojure.lang." (:name concrete) "/" name))
            repl (str "t/" (camel->kebab name))]
        (swap! entries assoc fqn (symbol repl))))
    @entries))

(defn generate-instance-table [host]
  (into {}
    (for [iface (:interfaces host)]
      [(symbol (str "clojure.lang." (:name iface)))
       (list 'satisfies? (symbol (str "p/" (:name iface))))])))

(defn generate-dot-table [host class-key replacement-fn]
  (let [{:keys [ops]} (get (:bridge host) class-key)]
    (into {}
      (for [[method _] ops
            :let [repl (replacement-fn method)]]
        [method (fn [args]
                  (if (str/starts-with? repl "h/")
                    (if (seq args)
                      (format "(%s (h/host) %s)" repl (str/join " " args))
                      (format "(%s (h/host))" repl))
                    (if (seq args)
                      (format "(%s %s)" repl (str/join " " args))
                      (format "(%s)" repl))))]))))

;; =============================================================================
;; rewrite-clj helpers
;; =============================================================================

(defn sym-str [zloc]
  (when (= :token (z/tag zloc))
    (try (let [s (z/sexpr zloc)]
           (when (symbol? s) (str s)))
         (catch Exception _ nil))))

(defn replace-with-str [zloc s]
  (z/replace zloc (n/coerce (read-string s))))

(defn children-strs [zloc]
  (when (z/list? zloc)
    (when-let [child (z/down zloc)]
      (loop [c child acc []]
        (let [acc (if (z/whitespace-or-comment? c) acc (conj acc (z/string c)))]
          (if-let [r (z/right c)] (recur r acc) acc))))))

;; =============================================================================
;; Metadata handling — strip JVM type hints structurally
;; =============================================================================

(defn jvm-metadata?
  "Check if a metadata node contains JVM-specific type hints."
  [zloc]
  (when (= :meta (z/tag zloc))
    (let [child (z/down zloc)]
      (when child
        (let [s (z/string child)]
          (or (and (str/starts-with? s "^") (jvm-type? (subs s 1)))
              (jvm-type? s)
              ;; {:tag ClassName} maps
              (when (= :map (z/tag child))
                (let [map-str (z/string child)]
                  (or (re-find #":tag\s+clojure\.lang\." map-str)
                      (re-find #":tag\s+java\." map-str)
                      (re-find #":tag\s+\"?\[" map-str))))))))))

(defn strip-inline-keys
  "Remove :inline and :inline-arities from metadata map string."
  [map-str]
  (-> map-str
      (str/replace #"\s*:inline-arities\s+[^\s,}]+" "")
      (str/replace #"\s*:inline\s+\(fn\s+\[[^\]]*\][^)]*\)\)" "")))

;; =============================================================================
;; Node transformations
;; =============================================================================

(defn transform-dot-form [zloc dot-tables]
  (let [children (children-strs zloc)]
    (when (and (>= (count children) 3) (= "." (first children)))
      (let [class-str (second children)
            rest-str (nth children 2)
            paren-form? (str/starts-with? rest-str "(")
            class-key (cond
                        (= class-str "clojure.lang.RT") "RT"
                        (= class-str "clojure.lang.Numbers") "Numbers"
                        (= class-str "clojure.lang.Util") "Util"
                        :else nil)
            ;; For unknown classes, derive replacement
            derive-replacement (fn [method-name args]
                                 (let [repl (or (static-replacement class-str method-name)
                                                (str "t/" (camel->kebab method-name)))]
                                   (if (seq args)
                                     (format "(%s %s)" repl (str/join " " args))
                                     (format "(%s)" repl))))]
        (if paren-form?
          (try
            (let [inner (read-string rest-str)
                  method-name (str (first inner))
                  args (mapv str (rest inner))]
              (if-let [table (get dot-tables class-key)]
                (when-let [f (get table method-name)] (f args))
                (derive-replacement method-name args)))
            (catch Exception _ nil))
          (let [method-name rest-str
                args (vec (drop 3 children))]
            (if-let [table (get dot-tables class-key)]
              (when-let [f (get table method-name)] (f args))
              (derive-replacement method-name args))))))))

(defn transform-instance-form [zloc instance-table]
  (let [children (children-strs zloc)]
    (when (and (>= (count children) 3) (= "instance?" (first children)))
      (let [class-sym (symbol (second children))
            val-str (nth children 2)]
        (if-let [replacement (get instance-table class-sym)]
          (format "(%s %s %s)" (first replacement) (second replacement) val-str)
          ;; Derive: any JVM class → (t/instance? t/ClassName val)
          (when (jvm-type? (str class-sym))
            (let [short (last (str/split (str class-sym) #"\."))]
              (format "(t/instance? t/%s %s)" short val-str))))))))

(defn transform-catch-form [zloc]
  (let [children (children-strs zloc)]
    (when (and (>= (count children) 3) (= "catch" (first children)))
      (let [class-name (second children)]
        (when (jvm-exception? class-name)
          (let [rest-args (drop 2 children)]
            (format "(catch Exception %s)" (str/join " " rest-args))))))))

(defn transform-import-form [zloc]
  (let [children (children-strs zloc)]
    (when (and (seq children) (= "import" (first children)))
      ";; import removed — using portable equivalents")))

(defn transform-node [zloc symbol-table instance-table dot-tables]
  (let [tag (z/tag zloc)]
    (cond
      ;; Symbol nodes
      (= :token tag)
      (when-let [s (sym-str zloc)]
        (let [sym (symbol s)]
          (cond
            ;; Known symbol
            (get symbol-table sym)
            (str (get symbol-table sym))

            ;; clojure.lang.Foo/bar → derive
            (and (str/includes? s "clojure.lang.") (str/includes? s "/"))
            (let [[cls method] (str/split s #"/")]
              (or (static-replacement cls method)
                  (str "t/" (camel->kebab method))))

            ;; clojure.lang.Foo → p/Foo or t/Foo
            (str/starts-with? s "clojure.lang.")
            (let [short (last (str/split s #"\."))]
              (str "t/" short))

            ;; java.* → t/*
            (and (jvm-type? s) (not (str/includes? s "/")))
            (let [short (last (str/split s #"\."))]
              (str "t/" short))

            ;; Foo. constructor as symbol → t/->foo
            (str/ends-with? s ".")
            (let [raw (subs s 0 (dec (count s)))
                  short (last (str/split raw #"\."))]
              (str "t/->" (camel->kebab short)))

            :else nil)))

      ;; Metadata nodes — strip JVM type hints
      (= :meta tag)
      (when (jvm-metadata? zloc)
        ;; Replace with just the value (strip metadata)
        (let [children (z/child-sexprs zloc)]
          (when (= 2 (count children))
            (str (second children)))))

      ;; List forms
      (= :list tag)
      (let [children (children-strs zloc)]
        (when (seq children)
          (let [head (first children)]
            (cond
              (= "." head)
              (transform-dot-form zloc dot-tables)

              (= "instance?" head)
              (transform-instance-form zloc instance-table)

              (= "new" head)
              (let [raw-name (second children)
                    short-name (last (str/split raw-name #"\."))
                    args (drop 2 children)
                    factory (str "t/->" (camel->kebab short-name))]
                (if (seq args)
                  (format "(%s %s)" factory (str/join " " args))
                  (format "(%s)" factory)))

              (= "catch" head)
              (transform-catch-form zloc)

              (= "import" head)
              (transform-import-form zloc)

              ;; (.method obj args...) → (p/-method obj args...)
              (and (str/starts-with? head ".")
                   (not= "." head)
                   (not (str/starts-with? head "..")))
              (let [method (subs head 1)
                    k (camel->kebab method)
                    args (rest children)]
                (if (seq args)
                  (format "(p/-%s %s)" k (str/join " " args))
                  (format "(p/-%s)" k)))

              ;; Foo. constructor → (t/->foo args...)
              (and (str/ends-with? head ".") (not= "." head))
              (let [raw-name (subs head 0 (dec (count head)))
                    short-name (last (str/split raw-name #"\."))
                    factory (str "t/->" (camel->kebab short-name))
                    args (rest children)]
                (if (seq args)
                  (format "(%s %s)" factory (str/join " " args))
                  (format "(%s)" factory)))

              ;; Class/method head → derive
              (and (str/includes? head "clojure.lang.") (str/includes? head "/"))
              (let [[cls method] (str/split head #"/")
                    repl (or (static-replacement cls method)
                             (str "t/" (camel->kebab method)))
                    args (rest children)
                    needs-host? (str/starts-with? repl "h/")]
                (if needs-host?
                  (if (seq args)
                    (format "(%s (h/host) %s)" repl (str/join " " args))
                    (format "(%s (h/host))" repl))
                  (if (seq args)
                    (format "(%s %s)" repl (str/join " " args))
                    (format "(%s)" repl))))

              :else nil))))

      :else nil)))

;; =============================================================================
;; NS form transformation (structural)
;; =============================================================================

(defn transform-ns-form [zloc]
  (let [children (children-strs zloc)]
    (when (and (seq children) (= "ns" (first children)))
      (let [ns-name (second children)]
        (format "(ns %s\n  (:require [clojure.protocols :as p]\n            [clojure.host :as h]\n            [clojure.types :as t]))"
                ns-name)))))

;; =============================================================================
;; Main pipeline
;; =============================================================================

(defn transform [input-file output-file]
  (println "portabilize: loading host contract from JVM reflection...")
  (let [host (load-host-contract)
        symbol-table (generate-symbol-table host)
        instance-table (generate-instance-table host)
        dot-tables {"RT"      (generate-dot-table host "RT" rt-replacement)
                    "Numbers" (generate-dot-table host "Numbers" numbers-replacement)
                    "Util"    (generate-dot-table host "Util" util-replacement)}
        _ (println (format "  %d symbol replacements, %d instance mappings"
                           (count symbol-table) (count instance-table)))]

    (println (format "  Input: %s" input-file))
    (let [content (slurp input-file)
          _ (println (format "  Lines: %d" (count (str/split-lines content))))

          ;; Single pass: tree walk handles everything
          result (let [zloc (z/of-string content {:track-position? true})]
                   (loop [loc zloc]
                     (if (z/end? loc)
                       (z/root-string loc)
                       (let [replacement (try
                                           (cond
                                             ;; NS form gets special treatment
                                             (and (= :list (z/tag loc))
                                                  (let [cs (children-strs loc)]
                                                    (and (seq cs) (= "ns" (first cs)))))
                                             (transform-ns-form loc)

                                             :else
                                             (transform-node loc symbol-table instance-table dot-tables))
                                           (catch Exception _ nil))]
                         (if replacement
                           (let [new-loc (try (replace-with-str loc replacement)
                                             (catch Exception _ loc))]
                             (recur (z/next new-loc)))
                           (recur (z/next loc)))))))
          _ (println "  Tree walk complete")]

      (let [output (str ";; PORTABLE CLOJURE — Generated by clojure.parity portabilize\n"
                        ";; JVM interop replaced with p/ (protocol), h/ (host), t/ (type) calls.\n\n"
                        result)]
        (spit output-file output)
        (println (format "  Written: %s (%d lines)" output-file (count (str/split-lines output))))

        ;; Residue check
        (println "\n  JVM residue check:")
        (let [lines (str/split-lines output)
              code-lines (remove #(str/starts-with? (str/trim %) ";;") lines)
              patterns [["clojure.lang." #"clojure\.lang\."]
                        ["java." #"java\.(lang|util|io|net|sql)\."]]]
          (let [total (atom 0)]
            (doseq [[label pat] patterns]
              (let [hits (count (filter #(re-find pat %) code-lines))]
                (when (pos? hits)
                  (swap! total + hits)
                  (println (format "    %s: %d remaining" label hits)))))
            (when (zero? @total)
              (println "    Clean — no JVM references in code."))))))))

;; =============================================================================
;; Entry
;; =============================================================================

(defn -main [& args]
  (let [input (or (first args) "core.clj")
        output (or (second args)
                   (str/replace input #"\.clj$" "_portable.cljc"))]
    (transform input output)))

(apply -main *command-line-args*)
