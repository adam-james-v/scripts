#!/usr/bin/env bb

(babashka.deps/add-deps
 '{:deps
   #_{scad-clj/scad-clj {:mvn/version "0.5.3"}}
   {scad-clj/scad-clj {:local/root "/Users/adam/dev/scad-clj"}}})

(ns clj-to-scad.main
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.data.xml :as xml]
            [svg-clj.path :as path]
            [svg-clj.utils :as utils]
            [svg-clj.transforms :as tf]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer [write-scad]]))

(defn clj-file->scad-block
  [fname]
  (let [[name ext] (str/split fname #"\.")]
    (if (= ext "scad")
      (slurp fname)
      (->> (slurp fname)
           (format "[%s]")
           load-string
           (filter (complement var?))))))

(defn png!
  [fname scad-block]
  (let [scad (write-scad [(fn! 150) scad-block])]
    (sh "openscad" "/dev/stdin"
        "--imgsize" "600,600"
        "--projection" "orthogonal"
        "--colorscheme" "greenscreen"
        "--camera" "0,0,0,55,0,25,2200"
        "-o" fname
        :in scad)))

(defn- anim-frames! [f name framerate dur]
  (let [mkdir (sh "mkdir" "-pv" name)
        frames (int (* framerate dur))
        framefn (fn [fr] (png! 
                          (format (str name "/%03d.png") fr)
                          (f (/ fr frames))))]
    (when (= 0 (:exit mkdir))
      (into [] (map framefn (range 1 (inc frames)))))))

(defn- anim-video! [name framerate]
  (let [ffmpeg 
        (sh "ffmpeg" "-f" "image2" "-r" (str framerate)
            "-i" (str name "/%03d.png")
            "-c:v" "libvpx-vp9"
            "-vf" "chromakey=0x00FF00:0.25:0.1"
            "-y" (str name ".webm"))]
    (when (= 0 (:exit ffmpeg))
      (sh "rm" "-rf" name))))

(defn animate! [{:keys [graphics-fn name framerate duration]}]
  (do (anim-frames! graphics-fn name framerate duration)
      (anim-video! name framerate)))

(defn img->str [fname]
  "Ingest image file `fname` and transform it into a hiccup data structure."
  (let [new-fname (str (first (str/split fname #"\.")) ".svg")]
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
  (let [group (svg-clj.main/g seq)
        ctr (mapv float (tf/centroid group))]
    (->> seq
         (map #(tf/translate (utils/v* [-1 -1] ctr) %)))))

(defn line
  [from to]
  (let [r 0.7]
    (color [0 0 0 1]
           (if (= from to)
             (sphere r)
             (let [diff (map - to from)
                   norm (utils/distance from to)
                   rotate-angle (Math/acos (/ (last diff) norm))
                   rotate-axis [(- (nth diff 1)) (nth diff 0) 0]]
               (union
                (sphere r)
                (translate to (sphere r))
                (->> (cylinder r norm)
                     (translate [0 0 (/ norm 2)])
                     (rotate rotate-angle rotate-axis)
                     (translate from))))))))

(defn polyline
  [pts]
  (apply union (map #(apply line %) (partition 2 1 pts))))

(defn- add-z
  [pts]
  (map #(conj % 0) pts))

(defn line-drawing
  [fname]
  (-> fname
      img->str
      str->elements
      re-center
      (->> (mapcat split-path))
      (->> (map path->pts))
      (->> (map add-z))
      (->> (map polyline))
      union))

(defn- flip-y
  [pts]
  (map #(utils/v* % [1 -1]) pts))

(defn svg-path-elem->scad-polygon
  [path-elem]
  (-> path-elem
      split-path
      (->> (map path->pts))
      (->> (map flip-y))
      (->> (map polygon))
      (->> (apply difference))))

(defn drawing
  [fname]
  (-> fname
      img->str
      str->elements
      #_re-center
      (->> (map svg-path-elem->scad-polygon))
      union))

(defn linecube
  [x y z]
  (union
   (color [0 1 0 1] (cube x y z))
   (translate [(/ x -2.0) (/ y -2.0) (/ z -2.0)]
    (union
     (line [0 0 0] [x 0 0])
     (line [x 0 0] [x y 0])
     (line [x y 0] [0 y 0])
     (line [0 y 0] [0 0 0])
     (line [0 0 0] [0 0 z])
     (line [x 0 0] [x 0 z])
     (line [x y 0] [x y z])
     (line [0 y 0] [0 y z])
     (line [0 0 z] [x 0 z])
     (line [x 0 z] [x y z])
     (line [x y z] [0 y z])
     (line [0 y z] [0 0 z])))))

(def cube-anim
  {:name "cube"
   :framerate 30
   :duration 4
   :graphics-fn
   (fn [t]
     (rotate [0 0 (* t (deg->rad 360))] (linecube 70 70 70)))})

(def draw-anim
  {:name "drawing"
   :framerate 30
   :duration 4
   :graphics-fn
   (fn [t]
     (rotate [0 0 (* t (deg->rad 360))]
             (->> (drawing "drawing2.png")
                  (rotate [(deg->rad 90) 0 0]))))})

(def vase-anim
  {:name "vase"
   :framerate 120
   :duration 4
   :graphics-fn
   (fn [t]
     (->> (drawing "drawing4.png")
          (extrude-rotate {:angle (* t 360)})
          (translate [0 0 300])))})

(defn build
  [fname]
  (let [fout (-> fname (str/split #"\.") first (str ".scad"))]
    (->> (slurp fname)
         (format "[%s]")
         load-string
         (filter (complement var?))
         write-scad
         (spit fout))))

#_(build (first *command-line-args*))
