#!/usr/bin/env bb

(babashka.deps/add-deps
 '{:deps
   {scad-clj/scad-clj {:local/root "/Users/adam/dev/scad-clj"}}})

(ns clj-to-step.main
  (:require [clojure.string :as st]
            [clojure.java.shell :refer [sh]]
            [scad-clj.model :refer :all]
            [scad-clj.csg :refer [write-csg]]))

(def this-directory (->> (sh "pwd")
                         :out
                         st/trim))

(defn exporter-script [ipath opath]
  (str "
import FreeCAD
import importCSG
import Import

App.newDocument(\"a\")
doc = FreeCAD.getDocument(\"a\")
importCSG.insert(u\"" ipath "\", \"a\")
__objs__ = doc.RootObjects
Import.export(__objs__, u\"" opath "\")
del __objs__"))

(defn scad->step
  [f scad-block]
  (let [path this-directory]
    (->> scad-block
         write-csg
         (spit "scadout.csg"))
    (->> (str path "/" f)
         (exporter-script (str path "/scadout.csg"))
         (spit "fcscript.py"))
    (sh "freecad" "fcscript.py")
    (sh "rm" "-rf" 
        "fcscript.py"
        "parsetab.py"
        "scadout.csg"
        "__pycache__")))

(defn build
  [f]
  (let [fout (-> f (st/split #"\.") first (str ".step"))]
    (->> f
         slurp
         (format "[%s]")
         load-string
         (filter (complement var?))
         (scad->step fout))))

#_(println this-directory)
(build (first *command-line-args*))
