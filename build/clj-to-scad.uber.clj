#!/usr/bin/env bb

(ns scad-clj.model
  (:refer-clojure :exclude [import use])
  (:require [clojure.walk :refer [postwalk]]
            #_( #?(:clj [scad-clj.text :refer [text-parts]]))
            #?(:clj [clojure.core.match :refer [match]]
               :cljs [cljs.core.match :refer-macros [match]])))

(def pi Math/PI)
(def tau (* 2 pi))

(defn rad->deg [radians]
  (/ (* radians 180) pi))

(defn deg->rad [degrees]
  (* (/ degrees 180) pi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special variables

(defn fa! [x]
  `(:fa ~x))

(defn fn! [x]
  `(:fn ~x))

(defn fs! [x]
  `(:fs ~x))

(def ^:dynamic *fa* false)
(def ^:dynamic *fn* false)
(def ^:dynamic *fs* false)
(def ^:dynamic *center* true)

(defn with-f* [f x block]
  `(binding [~f ~x]
     (postwalk identity (list ~@block))))

(defmacro with-fa [x & block]
  (with-f* 'scad-clj.model/*fa* x block))

(defmacro with-fn [x & block]
  (with-f* 'scad-clj.model/*fn* x block))

(defmacro with-fs [x & block]
  (with-f* 'scad-clj.model/*fs* x block))

(defmacro with-center [x & block]
  (with-f* 'scad-clj.model/*center* x block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifier

(defn modifier [modifier & block]
  (if (some #{modifier} [:# :% :* :!])
    `(:modifier ~(name modifier) ~@block)))

(defn -# [& block] (modifier :# block))
(defn -% [& block] (modifier :% block))
(defn -* [& block] (modifier :* block))
(defn -! [& block] (modifier :! block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include & call into Scad libraries

(defn import [file]
  `(:import ~file))

(defn include [library]
  `(:include {:library ~library}))

(defn use [library]
  `(:use {:library ~library}))

(defn libraries [& {uses :use includes :include}]
  (concat
   (map use uses)
   (map include includes)))

(defn call [function & args]
  `(:call {:function ~(name function)} ~args))

(defn call-module [module & args]
  `(:call-module-no-block {:module ~(name module)} ~args))
(defn call-module-with-block [module & args]
  `(:call-module-with-block {:module ~(name module)} ~args))
(defn define-module [module & body]
  `(:define-module {:module ~(name module)} ~body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2D

(defn square [x y & {:keys [center] :or {center *center*}}]
  `(:square ~{:x x, :y y, :center center}))

(defn circle [r]
  (let [args (merge {:r r}
                    (if *fa* {:fa *fa*})
                    (if *fn* {:fn *fn*})
                    (if *fs* {:fs *fs*}))]
    `(:circle ~args)))

(defn polygon
  ([points]
   `(:polygon {:points ~points}))
  ([points paths & {:keys [convexity]}]
   `(:polygon {:points ~points, :paths ~paths, :convexity ~convexity})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3D

(defn sphere [r]
  (let [args (merge {:r r}
                    (if *fa* {:fa *fa*})
                    (if *fn* {:fn *fn*})
                    (if *fs* {:fs *fs*}))]
    `(:sphere ~args)))

(defn cube [x y z & {:keys [center] :or {center *center*}}]
  `(:cube ~{:x x, :y y, :z z, :center center}))

(defn cylinder [rs h & {:keys [center] :or {center *center*}}]
  (let [fargs (merge (if *fa* {:fa *fa*})
                     (if *fn* {:fn *fn*})
                     (if *fs* {:fs *fs*}))]
    (match [rs]
      [[r1 r2]] `(:cylinder ~(merge fargs {:h h, :r1 r1, :r2 r2, :center center}))
      [r]       `(:cylinder ~(merge fargs {:h h, :r r, :center center})))))

(defn polyhedron
  ([points faces]
   `(:polyhedron {:points ~points :faces ~faces}))
  ([points faces & {:keys [convexity]}]
   `(:polyhedron {:points ~points :faces ~faces :convexity ~convexity})))


(defn group [& block]
  `(:group ~@block))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transformations

(defn resize [[x y z] & block]
  (let [is-auto (and (keyword? (first block))
                     (= :auto (first block)))
        auto (if is-auto (second block))
        block (if is-auto (rest (rest block)) block)]
    `(:resize {:x ~x :y ~y :z ~z :auto ~auto} ~@block)))

(defn translate [[x y z] & block]
  `(:translate [~x ~y ~z] ~@block))

; multi-arity can't have more than one signature with variable arity. '&'.
(defn rotatev [a [x y z] & block]
  `(:rotatev [~a [~x ~y ~z]] ~@block))

(defn rotatec [[x y z] & block]
  `(:rotatec [~x ~y ~z] ~@block))

(defn rotate [& block]
  (if (number? (first block))
    (rotatev (first block) (second block) (rest (rest block)))
    (rotatec (first block) (rest block))))

(defn scale [[x y z] & block]
  `(:scale [~x ~y ~z] ~@block))

(defn mirror [[x y z] & block]
  `(:mirror [~x ~y ~z] ~@block))

(defn color [[r g b a] & block]
  `(:color [~r ~g ~b ~a] ~@block))

(defn hull [ & block]
  `(:hull  ~@block))

(defn- offset-num
  "A narrow implementation of OpenSCAD’ offset() for radius only."
  [r & block]
  `(:offset {:r ~r} ~@block))

(defn- offset-map
  "A broad implementation of OpenSCAD’s offset(), supporting more parameters."
  [{:keys [r delta chamfer]} & block]
  `(:offset {:r ~r :delta ~delta :chamfer ~chamfer} ~@block))

(defn offset
  "Implement OpenSCAD’s offset() for two different call signatures."
  [options & block]
  (apply (partial (if (map? options) offset-map offset-num) options) block))

(defn minkowski [ & block]
  `(:minkowski ~@block))

(defn multmatrix [m & block]
  `(:multmatrix ~m ~@block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boolean operations

(defn union [ & block]
  `(:union  ~@block))

(defn intersection [ & block]
  `(:intersection  ~@block))

(defn difference [ & block]
  `(:difference  ~@block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other

(defn extrude-linear [{:keys [height twist convexity center slices scale] :or {center *center*}} & block]
  `(:extrude-linear {:height ~height :twist ~twist :convexity ~convexity :center ~center :slices ~slices :scale ~scale} ~@block))

(defn extrude-rotate
  ([block]
   (let [args (if *fn* {:fn *fn*} {})]
     `(:extrude-rotate ~args ~block)))
  ([{:keys [convexity angle]} block]
   (let [args (merge {:convexity convexity :angle angle}
                     (if *fn* {:fn *fn*} {}))]
     `(:extrude-rotate ~args ~block))))

(defn surface [filepath & {:keys [convexity center invert] :or {center *center*}}]
  `(:surface ~{:filepath filepath :convexity convexity :center center :invert invert}))

(defn projection [cut & block]
  `(:projection {:cut cut} ~@block))

(defn project [& block]
  `(:projection {:cut false} ~@block))

(defn cut [& block]
  `(:projection {:cut true} ~@block))

(defn render [& block]
  (if (and (seq block)
           (number? (first block)))
    (let [[c & bl] block]
      `(:render {:convexity ~c} ~@bl))
    `(:render {:convexity 1} ~@block)))

(defn excise
  "Like difference, but subtraction is from the last node, not the first."
  [& nodes]
  (difference (last nodes) (drop-last nodes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text

(defn text [text & {:as args}]
  (let [args (merge {:text text}
                    (if *fn* {:fn *fn*})
                    args)]
    `(:text ~args)))

#_(#?(:clj
    (defn polygon-text [font size text]
      (let [even-odd-paths (text-parts font size text)]
        (:shape
         (reduce (fn [{:keys [union? shape]} paths]
                   (if union?
                     {:union? false
                      :shape (apply union shape (map polygon paths))}
                     {:union? true
                      :shape (apply difference shape (map polygon paths))}))
                 {:union? true}
                 even-odd-paths))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extended

(defn extrude-curve [{:keys [height radius angle n]} block]
  (let [lim (Math/floor (/ n 2))
        phi (/ (/ angle (dec n)) 2)]
    (apply union
           (map (fn [x]
                  (let [theta (* 0.5 angle (/ x lim))
                        r radius
                        dx (* r (- (Math/sin theta)
                                   (* theta (Math/cos theta))))
                        dz (* r (+ (Math/cos theta)
                                   (* theta (Math/sin theta)) (- 1)))]
                    (translate [(+ dx (* 0 (Math/sin theta) (/ height 2)))
                                0
                                (+ dz (* 0 (Math/cos theta) (/ height 2)))]
                      (rotate theta [0 1 0]
                        (intersection
                         (translate [(* r theta) 0 0]
                           (cube (* 2 (+  r height) (Math/sin phi))
                                 1000 (* 2 height)))
                         (extrude-linear {:height height}
                           block))))))
                (range (- lim) (inc lim))))))
(ns scad-clj.scad
  (:require [clojure.string :refer [join]]
            [scad-clj.model :refer [rad->deg]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multimethod

(defmulti write-expr
  (fn [depth [form & args]]
    (if (keyword? form) form :list)))

(defmethod write-expr :default [depth [form & args]]
  `("//(" ~form ~args ")"))

(defmethod write-expr :list [depth [& args]]
  (mapcat #(write-expr depth %1) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility

(defn indent [depth]
  (join (repeat depth "  ")))

(defn write-block [depth block]
  (mapcat #(write-expr (inc depth) %1) block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifier

(defmethod write-expr :modifier [depth [form modifier & block]]
  (concat
   (list (indent depth) modifier "union () {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; include and call into scad libraries.

(declare map-to-arg-string)

(defn make-arguments [args]
  (let [arg (first args)
        rest (rest args)
        piece (cond
               (map? arg) (map-to-arg-string arg)
               (coll? arg) (str "[" (make-arguments arg) "]")
               :else arg)]
    (if (empty? rest)
      piece
      (join ", " [piece (make-arguments rest)]))))

(defn map-to-arg-string [m]
  (join ", " (map (fn [[k v]] (str (name k) "=" (make-arguments [v])) ) m)))

(defmethod write-expr :include [depth [form {:keys [library]}]]
  (list (indent depth) "include <" library">\n"))

(defmethod write-expr :use [depth [form {:keys [library]}]]
  (list (indent depth) "use <" library">\n"))

(defmethod write-expr :import [depth [form file]]
  (list (indent depth) "import (\"" file "\");\n"))

(defmethod write-expr :call [depth [form {:keys [function]} & args]]
  (list (indent depth) function "(" (make-arguments (apply vec args)) ");\n"))

(defmethod write-expr :call-module-with-block [depth [form {:keys [module]} & args]]
  (let [the-args (butlast (first args))
        block (list (last (first args)))]
    (concat
     (list (indent depth) module " (" (make-arguments (vec the-args)) ") {\n")
     (write-block depth block)
     (list (indent depth) "}\n"))))

(defmethod write-expr :call-module-no-block [depth [form {:keys [module]} & args]]
  (let [the-args (first args)]
    (list (indent depth) module " (" (make-arguments (vec the-args)) ");\n")))

(defmethod write-expr :define-module [depth [form {:keys [module]} & args]]
  (let [the-args (butlast (first args))
        block (list (last (first args)))]
    (concat
     (list (indent depth) "module " module "(" (make-arguments (vec the-args)) ") {\n")
     (write-block depth block)
     (list (indent depth) "};\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2D

(defmethod write-expr :circle [depth [form {:keys [r fa fn fs center]}]]
  (let [fargs (str (and fa (str "$fa=" fa ", "))
                   (and fn (str "$fn=" fn ", "))
                   (and fs (str "$fs=" fs ", ")))]
    (list (indent depth) "circle (" fargs "r=" r ");\n")))

(defmethod write-expr :square [depth [form {:keys [x y center]}]]
  (list (indent depth) "square ([" x ", " y "]"
        (when center ", center=true") ");\n"))

(defmethod write-expr :polygon [depth [form {:keys [points paths convexity]}]]
  `(~@(indent depth) "polygon ("
    "points=[[" ~(join "], [" (map #(join ", " %1) points)) "]]"
    ~@(when paths [", paths=[[" (join "], [" (map #(join "," %1) paths)) "]]"])
    ~@(when convexity [", convexity=" convexity])
    ");\n"))

(defmethod write-expr :text [depth [form {:keys [text size font halign valign spacing direction language script fn]}]]
  (list (indent depth) "text (\"" text "\""
        (when fn (str ", $fn=" fn))
        (when size (str ", size=" size))
        (when font (str ", font=\"" font "\""))
        (when halign (str ", halign=\"" halign "\""))
        (when valign (str ", valign=\"" valign "\""))
        (when spacing (str ", spacing=" spacing))
        (when direction (str ", direction=\"" direction "\""))
        (when language (str ", language=\"" language "\""))
        (when script (str ", script=\"" script "\""))");\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3D

(defmethod write-expr :sphere [depth [form {:keys [r fa fn fs center]}]]
  (let [fargs (str (and fa (str "$fa=" fa ", "))
                   (and fn (str "$fn=" fn ", "))
                   (and fs (str "$fs=" fs ", ")))]
    (list (indent depth) "sphere (" fargs "r=" r ");\n")))

(defmethod write-expr :cube [depth [form {:keys [x y z center]}]]
  (list (indent depth) "cube ([" x ", " y ", " z "]"
        (when center ", center=true") ");\n"))

(defmethod write-expr :cylinder [depth [form {:keys [h r r1 r2 fa fn fs center]}]]
  (let [fargs (str (and fa (str "$fa=" fa ", "))
                   (and fn (str "$fn=" fn ", "))
                   (and fs (str "$fs=" fs ", ")))]
    (concat
     (list (indent depth) "cylinder (" fargs "h=" h)
     (if r (list ", r=" r) (list ", r1=" r1 ", r2=" r2))
     (when center (list ", center=true"))
     (list ");\n"))))

(defmethod write-expr :polyhedron [depth [form {:keys [points faces convexity]}]]
  `(~@(indent depth) "polyhedron ("
    "points=[[" ~(join "], [" (map #(join ", " %1) points)) "]], "
    "faces=[[" ~(join "], [" (map #(join ", " %1) faces)) "]]"
    ~@(if (nil? convexity) [] [", convexity=" convexity])
    ");\n"))

(defmethod write-expr :group [depth [form & block]]
  (concat
   (list (indent depth) "group() {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transformations

(defmethod write-expr :resize [depth [form {:keys [x y z auto]} & block]]
  (concat
   (list (indent depth) "resize ([" x ", " y ", " z "]")
   (list (when-not (nil? auto)
           (str " auto="
                (if (coll? auto)
                  (str "[" (join ", " (map true? auto)) "]")
                  (true? auto)))))
   "){\n"
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :translate [depth [form [x y z] & block]]
  (concat
   (list (indent depth) "translate ([" x ", " y ", " z "]) {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :rotatev [depth [form [a [x y z]] & block]]
  (concat
   (list (indent depth) "rotate (a=" (rad->deg a) ", v=[" x ", " y ", " z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :rotatec [depth [form [x y z] & block]]
  (concat
   (list (indent depth) "rotate ([" (rad->deg x) "," (rad->deg y) "," (rad->deg z) "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :scale [depth [form [x y z] & block]]
  (concat
   (list (indent depth) "scale ([" x ", " y ", " z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :mirror [depth [form [x y z] & block]]
  (concat
   (list (indent depth) "mirror ([" x ", " y ", " z "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :color [depth [form [r g b a] & block]]
  (concat
    (list (indent depth) "color ([" r ", " g ", " b ", " a"]) {\n")
    (write-block depth block)
    (list (indent depth) "}\n")))

(defmethod write-expr :hull [depth [form & block]]
  (concat
   (list (indent depth) "hull () {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :offset
  [depth [form {:keys [r delta chamfer] :or {chamfer false}} & block]]
  (concat
   (list (indent depth) "offset (")
   (if r
     (list "r = " r)
     (list "delta = " delta))
   (when chamfer (list ", chamfer=true"))
   (list ") {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :minkowski [depth [form & block]]
  (concat
   (list (indent depth) "minkowski () {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :multmatrix [depth [form m & block]]
  (let [w (fn [s] (str "[" s "]")) ;; wrap
        co (fn [c] (apply str (interpose "," c)))] ;; put commas in between
    (concat
     (list (indent depth) "multmatrix(")
     (w (co (map #(w (co %)) m)))
     (list ") {\n")
     (mapcat #(write-expr (inc depth) %1) block)
     (list (indent depth) "}\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boolean operations

(defmethod write-expr :union [depth [form & block]]
  (concat
   (list (indent depth) "union () {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

(defmethod write-expr :difference [depth [form & block]]
  (concat
   (list (indent depth) "difference () {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :intersection [depth [form & block]]
  (concat
   (list (indent depth) "intersection () {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other

(defmethod write-expr :surface [depth [form {:keys [filepath convexity center invert]}]]
  (concat
   (list (indent depth) "surface (file = \"" filepath "\""
         (when convexity (format ", convexity=%d" convexity))
         (when center ", center=true")
         (when invert ", invert=true")
         ");\n")))

(defmethod write-expr :projection [depth [form {:keys [cut]} & block]]
  (concat
   (list (indent depth) "projection (cut = " cut ") {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :extrude-linear [depth [form {:keys [height twist convexity center slices scale]} & block]]
  (concat
   (list (indent depth) "linear_extrude (height=" height)
   (if (nil? twist) [] (list ", twist=" (rad->deg twist)))
   (if (nil? convexity) [] (list ", convexity=" convexity))
   (if (nil? slices) [] (list ", slices=" slices))
   (cond
     (nil? scale) []
     (sequential? scale) (list ", scale=[" (first scale) ", " (second scale) "]")
     :else (list ", scale=" scale))
   (when center (list ", center=true"))
   (list "){\n")

   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :extrude-rotate [depth [form {:keys [convexity fn angle]} & block]]
  (concat
   (list (indent depth) "rotate_extrude (")
   (join ", "
     (concat
       (if convexity [(str "convexity=" convexity)])
       (if angle [(str "angle=" angle)])
       (if fn [(str "$fn=" fn)])))
   (list ") {\n")
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

(defmethod write-expr :render [depth [form {:keys [convexity]} & block]]
  (concat
   (list (indent depth) (str "render (convexity=" convexity ") {\n"))
   (mapcat #(write-expr (inc depth) %1) block)
   (list (indent depth) "}\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special variables

(defmethod write-expr :fa [depth [form x]]
  (list (indent depth) "$fa = " x ";\n"))

(defmethod write-expr :fn [depth [form x]]
  (list (indent depth) "$fn = " x ";\n"))

(defmethod write-expr :fs [depth [form x]]
  (list (indent depth) "$fs = " x ";\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; output

(defn write-scad [& block]
  (join (write-expr 0 block)))
(ns clj-to-scad.main (:require [clojure.string :as st] [scad-clj.model :refer :all] [scad-clj.scad :refer [write-scad]]))

(defn build [f] (let [fout (-> f (st/split #"\.") first (str ".scad"))] (->> (slurp f) (format "[%s]") load-string (filter (complement var?)) write-scad (spit fout))))

(build (first *command-line-args*))