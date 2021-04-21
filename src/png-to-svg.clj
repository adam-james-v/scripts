#!/usr/bin/env bb

(babashka.deps/add-deps
 '{:deps
   {borkdude/spartan.spec {:git/url "https://github.com/borkdude/spartan.spec"
                           :sha "e5c9f40ebcc64b27b3e3e83ad2a285ccc0997097"}
    org.clojure/math.combinatorics {:mvn/version "0.1.6"}
    svg-clj/svg-clj {:local/root "/Users/adam/dev/svg-clj"}
    #_{:git/url "https://github.com/adam-james-v/svg-clj"
                     :sha "aaf78937d7a59e11aa7b193c2f9da35d9d159ca6"}}
   :classpath-overrides {org.clojure/clojure nil
                         org.clojure/spec.alpha nil
                         org.clojure/core.specs.alpha nil}})

(ns png-to-svg.main
  (:require [spartan.spec :as s]
            [clojure.string :as st]
            [clojure.math.combinatorics :as combo]
            [clojure.java.shell :refer [sh]]
            [svg-clj.main :refer :all]
            [svg-clj.utils :as utils]
            [svg-clj.specs :as specs]
            [svg-clj.transforms :as tf]
            [hiccup.core :refer [html]]))
(require '[spartan.spec])
(alias 's 'clojure.spec.alpha)

;; suggestion from Michiel @borkdude on stream
(require '[clojure.spec.alpha :as s])

(defn img->str [fname]
  "Ingest image file `fname` and transform it into a hiccup data structure."
  (let [new-fname (str (first (st/split fname #"\.")) ".svg")]
    (sh "vtracer" 
        "--mode" "polygon"
        "--colormode" "bw"
        "--input" fname
        "--output" new-fname)
    (let [svg-str (slurp new-fname)]
      (sh "rm" new-fname)
      (-> svg-str
          (st/replace #"<\?xml.+>" "")
          st/trim))))

(defn str->elements
  [str]
  (->> str
       ->edn
       (drop 2)))


(defn tr
  "Translates the `elems` by `x` and `y` relative to the element(s)'s current position(s).

  For example, a shape sitting at [10 10] being translated by [10 10] will be located at [20 20] after translation."
  [[x y] & elems]
  (let [elem (first elems)
        elems (rest elems)]
    (when elem
      (cond
        (and (specs/element? elem) (= 0 (count elems)))
        (tf/translate-element [x y] elem)
        
        #_(and (specs/element? elem) (< 0 (count elems)))
        #_(concat
         [(tf/translate-element [x y] elem)]
         [(tr [x y] elems)])
      
        #_:else
        #_f(recur [x y] (concat elem elems))))))


(defn re-center
  [seq]
  (let [group (g seq)
        ctr (mapv float (centroid group))]
    (->> seq
         (map #(tf/translate-element (utils/v* [-1 -1] ctr) %)))))

(defn build
  [f]
  (let [fout (-> f (st/split #"\.") first (str ".svg"))]
    (->> f
         img->str
         str->elements
         re-center
         #_(svg [2048 2048 1]))))

(build (first *command-line-args*))
