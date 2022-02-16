#!/usr/bin/env bb

(babashka.deps/add-deps
 '{:deps
   {borkdude/spartan.spec {:git/url "https://github.com/borkdude/spartan.spec"
                           :sha "e5c9f40ebcc64b27b3e3e83ad2a285ccc0997097"}
    org.clojure/math.combinatorics {:mvn/version "0.1.6"}
    svg-clj/svg-clj {:local/root "/Users/adam/dev/svg-clj"}}})

(ns png-to-svg.main
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.java.shell :refer [sh]]
            [clojure.data.xml :as xml]
            [svg-clj.main :refer :all]
            [svg-clj.path :as path]
            [svg-clj.utils :as utils]
            [svg-clj.transforms :as tf]
            [hiccup.core :refer [html]]))

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
          (str/replace #"<\?xml.+>" "")
          str/trim))))

;; xml parse/transform technique is from:
;; https://github.com/babashka/babashka/blob/master/examples/portal.clj

(defn xml->hiccup [xml]
  (if-let [t (:tag xml)]
    (let [elt [t]
          elt (if-let [attrs (:attrs xml)]
                (conj elt attrs)
                elt)]
      (into elt (map xml->hiccup (:content xml))))
    xml))

(defn str->elements
  [str]
  (-> str
      (xml/parse-str :namespace-aware false
                     :skip-whitespace true)
      xml->hiccup
      (->> (drop 2))))

(defn split-path
  [[k props]]
  (let [ps (-> (:d props)
               (str/split #"(?=M)")
               (->> (map str/trim)))]
    (map #(assoc-in [k props] [1 :d] %) ps)))

(defn path->pts
  [path-elem]
  (let [cmds (path/path-string->commands (get-in path-elem [1 :d]))]
    (mapv :input cmds)))

#_(-> "drawing.png"
      img->str
      str->elements
      (->> (mapcat split-path)))

(defn re-center
  [seq]
  (let [group (g seq)
        ctr (mapv float (tf/centroid group))]
    (->> seq
         (map #(tf/translate (utils/v* [-1 -1] ctr) %)))))

(defn build
  [f]
  (let [fout (-> f (st/split #"\.") first (str ".svg"))]
    (->> f
         img->str
         str->elements
         re-center
         #_(svg [2048 2048 1]))))

(build (first *command-line-args*))
