#!/usr/bin/env bb

(babashka.deps/add-deps
 '{:deps
   {scad-clj/scad-clj {:mvn/version "0.5.3"}}
   #_{scad-clj/scad-clj {:local/root "/Users/adam/dev/scad-clj"}}})

(ns clj-to-scad.main
  (:require [clojure.string :as st]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer [write-scad]]))

(defn build
  [f]
  (let [fout (-> f (st/split #"\.") first (str ".scad"))]
    (->> (slurp f)
         (format "[%s]")
         load-string
         (filter (complement var?))
         write-scad
         (spit fout))))

(build (first *command-line-args*))
