#!/usr/bin/env bb

(babashka.deps/add-deps
 '{:deps
   {svg-clj/svg-clj {:local/root "/Users/adam/dev/svg-clj"}
    metasoarous/oz  {:mvn/version "2.0.0-alpha5"}}})

(ns svgl.main
  "Use svg-clj with file watching and live reloading."
  (:require [oz.core :as oz]))

(oz/start-server! 10666)
(oz/live-view! (first *command-line-args*))
