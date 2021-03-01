#!/usr/bin/env bb

(babashka.deps/add-deps
 '{:deps
   {svg-clj/scad-clj {:local/root "/Users/adam/dev/scad-clj"}
    svg-clj/svg-clj {:local/root "/Users/adam/dev/svg-clj"}
    borkdude/spartan.spec {:git/url "https://github.com/borkdude/spartan.spec"
                               :sha "e5c9f40ebcc64b27b3e3e83ad2a285ccc0997097"}}})

(ns scad-slice.main
  (:require [spartan.spec]
            [clojure.spec.alpha :as s]
            [scad-clj.model :refer :all]
            [scad-clj.scad :refer [write-scad]]
            [svg-clj.main :as svg]
            [hiccup.core :refer [html]]
            [clojure.string :as st]
            [clojure.java.shell :refer [sh]]))

(defn f->path
  [f]
  (->> (slurp f)
       st/split-lines
       (drop 2)
       (apply str)
       svg/->edn
       svg/unwrap-elements))

(defn to-deg
  [rad]
  (* rad (/ 180 Math/PI)))

(defn to-rad
  [deg]
  (* deg (/ Math/PI 180)))

(defn round
  [num places]
  (let [d (Math/pow 10 places)]
    (/ (Math/round (* num d)) d)))

(defn sin-cos-pair [theta]
  [(Math/sin (to-rad theta)) (Math/cos (to-rad theta))])

(defn rot-pt-2d
  [[x y] theta]
  (let [[s-t c-t] (sin-cos-pair theta)]
    [(- (* x c-t) (* y s-t))
     (+ (* y c-t) (* x s-t))]))

;; this rotates a point around [0,0,0]
(defn rot-pt
  [[x y z] axis theta]
  (cond
    (= axis :x) (into [x] (rot-pt-2d [y z] theta))
    (= axis :y) (apply #(into [] [%2 y %1]) (rot-pt-2d [z x] theta))
    (= axis :z) (into (rot-pt-2d [x y] theta) [z])))

(defn rotate-point
  [pt [ax ay az]]
  (-> pt
      (rot-pt :z az)
      (rot-pt :y ay)
      (rot-pt :x ax)))

(defn rotate-points
  [pts [ax ay az]]
  (mapv #(rotate-point % [ax ay az]) pts))

(def iso-euler-angles [35.264 45 0])
(def origin-angle-adjust-a [90 0 0])
(def origin-angle-adjust-b [0 -90 0])

(defn isometric-xf
  [pts]
  (-> pts
      (rotate-points origin-angle-adjust-a)
      (rotate-points origin-angle-adjust-b)
      (rotate-points iso-euler-angles)))

(defn top-xf
  [pts]
  (-> pts
      (rotate-points [0 0 0])))

(defn right-xf
  [pts]
  (-> pts
      (rotate-points [90 0 0])))

(defn add-z [pt2d] (vec (concat pt2d [0])))
(defn drop-z [pt3d] (vec (take 2 pt3d)))
(defn round-pt [places pt] (mapv #(round % places) pt))

(defn path->path-pts
  [path]
  (->> (get-in path [1 :d])
       svg/path-string->commands
       (partition-by #(= "Z" (:command %)))
       (partition 2)
       (map vec)
       (map #(apply concat %))
       (map #(map :input %))
       (mapv #(vec (filter (complement nil?) %)))))

(defn re-zero
  [path]
  (let [[[minx miny] _ _ _] (svg/bounds path)]
    (svg/translate [(- minx) (- miny)] path)))

(defn bb-dims
  [path]
  (let [[[minx miny] _ [maxx maxy] _] (svg/bounds path)]
    [(int (Math/ceil (- maxx minx)))
     (int (Math/ceil (- maxy miny)))]))

(defn pts-bb-area
  [pts]
  (->> pts
       svg/polygon-path
       bb-dims
       (apply *)))

(defn remove-largest
  [n path]
  (let [ctr (svg/centroid path)
        rem (->> path
                 path->path-pts
                 (sort-by pts-bb-area)
                 (drop-last n))]
    (if (< 0 (count rem))
      (->> rem
           (map svg/polygon-path)
           (apply svg/merge-paths)
           vec)
      (svg/polygon-path [ ctr ]))))

(defn path->iso-path
  [path]
  (->> path
       path->path-pts
       (map #(mapv add-z %))
       (map isometric-xf)
       (map #(mapv drop-z %))
       (map #(mapv (partial round-pt 5) %))
       (map svg/polygon-path)
       (apply svg/merge-paths)
       vec
       re-zero
       (remove-largest 2)
       (svg/style {:fill "none"
                   :stroke "slategray"
                   :stroke-width "0.5px"})))

(defn vstack
  [gap elems]
  (let [elems (vec elems)]
    (->>
     (apply svg/g
            (for [y (reverse (range (count elems)))]
              (svg/translate [0 (* y gap)]
                             (get elems (- (dec (count elems)) y)))))
     re-zero)))

(defn slices->svg
  [gap slices]
  (->> slices
       (map path->iso-path)
       (vstack gap)
       (#(svg/svg (concat (bb-dims %) [1]) %))
       html
       (spit "iso-slices.svg")))

(defn scad->slice-anim
  [scad-block]
  (let [st 5]
    (pmap
      (fn [a]
        (do
         (->> scad-block
              (union (difference
                      (cube 1000 1000 1000)
                      (cube 999 999 1100)))
              (translate [0 0 (- a)])
              (scale [25.4 25.4 25.4])
              cut
              write-scad
              (spit (format "slice/%03d.scad" (int (/ a st)))))
         (sh "openscad" (format "slice/%03d.scad" (int (/ a st)))
             "-o" (format "slice/%03d.svg" (int (/ a st))))))
      (range 0 110 st))
    #_(->> (range 0 (int (/ 110 5)))
         (map #(format "slice/%03d.svg" %))
         (map f->path)
         (map first)
         (slices->svg st))))

(defn merge-slice-files
  []
  (->> (range 0 (int (/ 110 5)))
       (map #(format "slice/%03d.svg" %))
       (map f->path)
       (map first)
       (slices->svg 5)))

(defn clj->slice-anim
  [f]
  (->> f
       slurp
       (format "[%s]")
       load-string
       (filter (complement var?))
       scad->slice-anim)
  merge-slice-files)

(defn elem?
  [elem]
  (or (s/valid? :svg-clj.main/basic-element elem)
      (s/valid? :svg-clj.main/text-element elem)
      (s/valid? :svg-clj.main/g-element elem)
      (s/valid? :svg-clj.main/path-element elem)))

#_(clj->slice-anim (first *command-line-args*))
#_(merge-slice-files)

(->> "slice/006.svg"
     f->path
     first
     (#(get-in % [1 :d]))
     (s/explain :svg-clj.main/path-string)
     println)
