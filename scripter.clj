(ns scripter.core
  (:require [babashka.classpath :refer [add-classpath]]
            [clojure.string :as st]
            [clojure.java.shell :refer [sh]]))

(defn split-fname
  [fname]
  (let [sf (st/split fname #"\.")]
    [(apply str (drop-last sf))
     (last sf)]))

(defn split-folders
  [name]
  (let [sf (st/split name #"/")]
    (vec sf)))

(defn move
  [f]
  (let [[name ext] (split-fname f)
        sname (split-folders name)
        xfname (if (= (first sname) "src")
                 (apply str (conj (rest sname) "build/"))
                 (apply str (interpose "/" sname)))
        fout (str xfname "." ext)]
    (println fout)
    (sh "chmod" "+x" fout)))

(defn main
  []
  (let [f (first *command-line-args*)]
    (println (str "Moving script: " f))
    (move f)
    #_(println (get-deps f))))

(main)
