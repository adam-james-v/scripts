(ns uberscripter.core
  (:require [babashka.classpath :refer [add-classpath]]
            [clojure.string :as st]
            [clojure.java.shell :refer [sh]]))

(defn get-deps
  [f]
  (->> (slurp f)
       (format "[%s]")
       read-string
       first
       second
       second
       str
       (sh "clojure" "-Spath" "-Sdeps")
       :out
       st/trim))

(defn remove-deps
  [f]
  (->> (slurp f)
       (format "[%s]")
       read-string
       rest
       (interpose "\n\n")
       (apply str)))

(defn split-fname
  [fname]
  (let [sf (st/split fname #"\.")]
    [(apply str (drop-last sf))
     (last sf)]))

(defn split-folders
  [name]
  (let [sf (st/split name #"/")]
    (vec sf)))

(defn build
  [f]
  (let [[name ext] (split-fname f)
        sname (split-folders name)
        xfname (if (= (first sname) "src")
                 (apply str (conj (rest sname) "build/"))
                 (apply str (interpose "/" sname)))
        fout (str xfname ".uber." ext)
        ftemp (str f ".tmp")]
    (spit ftemp (remove-deps f))
    (sh "bb" "--classpath" (get-deps f)
        "-f" ftemp "--uberscript" fout)
    (sh "rm" ftemp)
    (->> (slurp fout)
         (str "#!/usr/bin/env bb\n\n")
         (spit fout))
    (println fout)
    (sh "chmod" "+x" fout)))

(defn main
  []
  (let [f (first *command-line-args*)]
    (println (str "Building script: " f))
    (build f)
    #_(println (get-deps f))))

(main)
