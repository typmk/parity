(ns parity.analyze
  "Source analysis + JVM host contract + implementation roadmap.

  Three functions:
    reflect  — JVM class reflection (what Clojure requires from the host)
    deps     — source dependency graph (what .clj code uses)
    roadmap  — merged priority tree (what to implement first)"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [parity.analyze.roots :as roots]
            [parity.analyze.branch :as branch]
            [parity.analyze.tree :as tree]
            [parity.color :refer [*color*]]))

(defn reflect
  "JVM host contract: what interfaces, methods, types Clojure requires."
  [& {:keys [edn? no-color?]}]
  (let [host (roots/collect-host)]
    (binding [*color* (not no-color?)]
      (if edn?
        (prn host)
        (roots/print-langmap host)))))

(defn deps
  "Source-level dependency graph from Clojure .clj files."
  [input & {:keys [mode no-color?]}]
  (binding [*color* (not no-color?)]
    (let [{:keys [classified layers file-results]} (branch/analyze-source input)]
      (case (or mode :summary)
        :summary  (branch/print-summary classified layers file-results)
        :host     (branch/print-host-contract classified)
        :host-edn (branch/print-host-data classified)
        :edn      (branch/print-edn classified layers file-results)
        :dot      (branch/print-dot classified))
      {:classified classified :layers layers :file-results file-results})))

(defn roadmap
  "What to implement next. Merges source deps + JVM host contract."
  [clojure-src & {:keys [edn? no-color?]}]
  (binding [*color* (not no-color?)]
    (let [{:keys [classified layers file-results]} (branch/analyze-source clojure-src)
          host-data (roots/collect-host)]
      (let [max-layer (if (empty? (vals layers)) 0 (apply max (vals layers)))
            graph-edn {:files (count file-results)
                       :definitions (count classified)
                       :max-layer max-layer
                       :graph (into (sorted-map)
                                    (map (fn [[qn info]]
                                           [qn {:ns (:ns info)
                                                :name (:name info)
                                                :kind (:kind info)
                                                :layer (get layers qn 0)
                                                :class (:class info)
                                                :deps (vec (sort (:deps info)))
                                                :dependents (:dependents info)
                                                :host-refs (vec (sort (:host-refs info)))
                                                :host-parsed (vec (for [p (sort-by :raw (:host-parsed info))]
                                                                    (select-keys p [:pkg :class :member])))}]))
                                    classified)}
            enriched (tree/merge-and-enrich graph-edn host-data)]
        (if edn?
          (tree/print-edn-tree enriched host-data)
          (tree/print-tree enriched host-data))))))
