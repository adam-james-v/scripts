#!/usr/bin/env bb

(babashka.deps/add-deps
 '{:deps
   #_{scad-clj/scad-clj {:mvn/version "0.5.3"}}
   {svg-clj/svg-clj {:local/root "/Users/adam/dev/svg-clj"}
    scad-clj/scad-clj {:local/root "/Users/adam/dev/scad-clj"}}})

(ns clj-to-scad.main
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.data.xml :as xml]
            [svg-clj.main :as svg]
            [svg-clj.path :as path]
            [svg-clj.utils :as utils]
            [svg-clj.transforms :as tf]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer [write-scad]]
            [scad-clj.csg :refer [write-csg]]))

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
  (let [scad (write-scad [(fn! 20) scad-block])]
    (sh "openscad" "/dev/stdin"
        "--imgsize" "1200,1200"
        "--projection" "orthogonal"
        "--colorscheme" "greenscreen" #_"DeepOcean"
        "--camera" "0,0,0,55,0,25,2900"
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
        "--segment_length" "3.5"
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
  (let [group (svg/g seq)
        ctr (mapv float (tf/centroid group))]
    (->> seq
         (map #(tf/translate (utils/v* [-1 -1] ctr) %)))))

(defn line
  [from to]
  (let [r 2.75]
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

(defn- flip-y
  [pts]
  (map #(utils/v* % [1 -1]) pts))

(defn line-drawing
  [fname]
  (-> fname
      img->str
      str->elements
      re-center
      (->> (mapcat split-path))
      (->> (map path->pts))
      (->> (map flip-y))
      (->> (map add-z))
      (->> (map polyline))
      union))

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
      re-center
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

(def wing-col (mapv #(/ % 255.0) [0 255 0 255]))
(def wing-col2 (mapv #(/ % 255.0) [0 255 0 255]))
(def wing-col3 (mapv #(/ % 255.0) [0 255 0 255]))
(def body-col (mapv #(/ % 255.0) [0 255 0 255]))

(def moth-eyes
  (union
   (->> (line-drawing "moth-sk/eyes.png")
        (rotate [(deg->rad 25) 0 0])
        (translate [0 220 43]))
   (->> (drawing "moth-sk/eyes.png")
        (extrude-linear {:height 5})
        (rotate [(deg->rad 25) 0 0])
        (translate [0 220 43])
        (color body-col))))

(def moth-body
  (let [sk (union
            (line-drawing "moth-sk/body.png")
            (->> (drawing "moth-sk/body.png")
                 (extrude-linear {:height 3})
                 (color body-col)))]
    (union
     moth-eyes
     (rotate
      [0 (deg->rad 30) 0]
      sk
      (rotate [0 (deg->rad 60) 0] sk)
      (rotate [0 (deg->rad -60) 0] sk)))))

(def right-wing
  (->>
   (union
    (line-drawing "moth-sk/right-wing.png")
    (->> (drawing "moth-sk/right-wing-outline.png")
         (extrude-linear {:height 1})
         (translate [-30 -60 0])
         (color wing-col3))
    (->> (drawing "moth-sk/right-wing.png")
         (extrude-linear {:height 6})
         (color wing-col))
    (->> (drawing "moth-sk/right-wing-shading.png")
         (extrude-linear {:height 4})
         (rotate [(deg->rad 0) 0 0])
         (translate [-24 52 0])
         (color wing-col2)))
   (translate [275 40 0])))

(def left-wing
  (->>
   (union
    (line-drawing "moth-sk/left-wing.png")
    (->> (drawing "moth-sk/left-wing-outline.png")
         (extrude-linear {:height 1})
         (translate [14 -38 0])
         (color wing-col3))
    (->> (drawing "moth-sk/left-wing.png")
         (extrude-linear {:height 6})
         (color wing-col))
    (->> (drawing "moth-sk/left-wing-shading.png")
         (extrude-linear {:height 4})
         (rotate [(deg->rad 0) 0 0])
         (translate [-51 136 0])
         (color wing-col2)))
   (translate [-245 -5 0])))

(def moth
  (union
   moth-body
   moth-eyes
   (->> right-wing (rotate [0 (deg->rad -20) 0]))
   (->>  left-wing (rotate [0 (deg->rad 20) 0]))))

#_(spit "cube.scad" (write-scad moth))

(defn ease-in-out-cubic [t]
  (if (< t 0.5)
    (* 4 t t t)
    (- 1 (/ (Math/pow (+ 2 (* t -2)) 3) 2))))

(def moth-anim
  {:name "mothline"
   :framerate 60
   :duration 5
   :graphics-fn
   (fn [t]
     (let [nt (* t 2 Math/PI)]
       (translate
        [0 0 -150]
        (rotate
         [0 0 (* t (deg->rad 360))]
         (union
          moth-body
          moth-eyes
          (->> right-wing
               (rotate [0 (deg->rad -18) 0])
               (rotate [0 (deg->rad (* -60 (Math/sin nt))) 0]))
          (->> left-wing
               (rotate [0 (deg->rad 18) 0])
               (rotate [0 (deg->rad (* 60 (Math/sin nt))) 0])))))))})

(animate! moth-anim)

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
