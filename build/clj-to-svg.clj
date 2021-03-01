#!/usr/bin/env bb

(babashka.deps/add-deps
 '{:deps
   {borkdude/spartan.spec {:git/url "https://github.com/borkdude/spartan.spec"
                               :sha "e5c9f40ebcc64b27b3e3e83ad2a285ccc0997097"}
    svg-clj/svg-clj {:local/root "/Users/adam/dev/svg-clj"}}
   :classpath-overrides {org.clojure/clojure nil
                         org.clojure/spec.alpha nil
                         org.clojure/core.specs.alpha nil}})

#_(require '[clojure.string :as st]
         '[svg-clj.main :refer :all]
         '[hiccup.core :refer [html]])


(ns clj-to-svg.main
  (:require [spartan.spec :as s]
            [clojure.string :as st]
            [svg-clj.main :refer :all]
            [hiccup.core :refer [html]]))
(require '[spartan.spec])
(alias 's 'clojure.spec.alpha)

;; suggestion from Michiel @borkdude on stream
(require '[clojure.spec.alpha :as s])

(defn build
  [f]
  (let [fout (-> f (st/split #"\.") first (str ".svg"))]
    (->> (slurp f)
         (format "[%s]")
         load-string
         (filter (complement var?))
         html
         (spit fout))))

(build (first *command-line-args*))
