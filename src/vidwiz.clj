#!/usr/bin/env bb

(babashka.deps/add-deps
 '{:deps
   {borkdude/spartan.spec {:git/url "https://github.com/borkdude/spartan.spec"
                               :sha "e5c9f40ebcc64b27b3e3e83ad2a285ccc0997097"}
    svg-clj/svg-clj {:local/root "/Users/adam/dev/svg-clj"}}})

(ns vidwiz.main
  "This is a prototype script for automating a portion of my video editing using ffmpeg."
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [hiccup.core :refer [html]]
            [cheshire.core :refer [parse-string]]
            [svg-clj.main :as svg]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]))

;; utils
(defn get-extension
  [fname]
  (re-find #"\.[A-Za-z\d+]+" fname))

(defn get-resolution-old
  [fname]
  (->> (sh "ffprobe" "-i" fname)
       :err
       (re-find #"\d\d+x\d+")
       (#(str/split % #"x"))
       (mapv read-string)))

(defn get-resolution
  [fname]
  (when-let [{:keys [width height]}
             (-> (sh "ffprobe"
                     "-v"
                     "error"
                     "-select_streams"
                     "v:0"
                     "-show_entries"
                     "stream=width,height"
                     "-of" "json"
                     fname)
                 :out
                 (parse-string true)
                 :streams
                 first)]
    [width height]))

(defn get-duration
  [fname]
  (when-let [{:keys [duration]}
             (-> (sh "ffprobe"
                     "-v"
                     "error"
                     "-select_streams"
                     "v:0"
                     "-show_entries"
                     "stream=duration"
                     "-of" "json"
                     fname)
                 :out
                 (parse-string true)
                 :streams
                 first)]
    (read-string duration)))

(defn overlay-offsets
  [{:keys [border base-dims overlay-dims pos gap fname]}]
  (let [{:keys [width]} border
        [cw ch] (map #(+ (* 2 width) %) overlay-dims)
        {:keys [h v]} pos
        [sw sh] base-dims]
    [(cond (= h :l) gap
           (= h :c) (- (/ sw 2) (/ cw 2))
           (= h :r) (- sw gap cw))
     (cond (= v :t) gap
           (= v :c) (- (/ sh 2) (/ ch 2))
           (= v :b) (- sh gap ch))]))

(defn get-bg-color
  [fname]
  (let [nfname (str/replace fname (get-extension fname) ".png")]
    (sh "ffmpeg" "-i" fname
        "-frames:v" "1"
        "-filter_complex"
        (str "[0:v]crop=4:4:100:500")
        "-y" (str nfname))
    (sh "convert" nfname "-colors" "1" nfname)
    (let [col (->> (sh "identify" "-verbose" nfname)
                   :out
                   (str/split-lines)
                   (drop-while #(not (str/includes? % "Histogram")))
                   (second)
                   (re-find #"\#......"))]
      (sh "rm" nfname)
      col)))

(defn seconds->timestamp [s]
  (let [hh (format "%02d" (int (/ s 3600)))
        mm (format "%02d" (int (/ (rem s 3600) 60)))
        ss (format "%02d" (rem s 60))]
    (apply str (interpose ":" [hh mm ss]))))

(defn clean-name [name]
  (-> name
      (str/lower-case)
      (str/replace #"!" "")
      (str/replace #"'" "")
      (str/replace #"," "")
      (str/replace #"/" "-")
      (str/replace #"\|" "-")
      (str/replace #"\\" "-")
      (str/replace #"&" "and")
      (str/replace #" " "-")))

(defn parse-url [url]
  (let [[xurl & rest] (str/split url #"[\?&]")
        xr (map #(str/split % #"=") rest)
        keys (map keyword (into [:full-url :url] (map first xr)))
        vals (into [url xurl] (map second xr))]
    (zipmap keys vals)))

(defn yt-url->video-data [url]
  (let [[title video-url audio-url]
        (-> (sh "youtube-dl" 
                "--get-title" "--youtube-skip-dash-manifest" "-g" url)
            (:out)
            (str/split-lines))]
    {:title title
     :audio-url audio-url
     :video-url video-url}))

(defn save-clip! [video-url audio-url time dur fname]
  (sh "ffmpeg" 
      "-ss" (str time)
      "-i" video-url
      "-ss" (str time)
      "-i" audio-url
      "-map" "0:v" "-map" "1:a"
      "-t" (str dur)
      "-y" fname))

(defn clip!
  ([name url duration]
   (let [urlp (parse-url url)
         data (yt-url->video-data (:full-url urlp))
         video-url (:video-url data)
         audio-url (:audio-url data)
         start-time (seconds->timestamp (read-string (:t urlp)))
         fname (str name ".mov")]
     (save-clip! video-url audio-url start-time duration fname)))
  
  ([name url start-time duration]
   (let [urlp (parse-url url)
         data (yt-url->video-data (:full-url urlp))
         video-url (:video-url data)
         audio-url (:audio-url data)
         fname (str name ".mov")]
     (save-clip! video-url audio-url start-time duration fname))))

(defn png! [fname svg-data]
  (sh "convert" "-background" "none" "/dev/stdin" fname
      :in (html svg-data)))

;; z-index suggested by maacl72
(defn- layer-input-partial [{:keys [file]}]
  (let [[name ext] (str/split file #"\.")]
    (concat 
     (when (= ext "webm") ["-vcodec" "libvpx-vp9"])
     ["-i" file])))

(defn- layer-input
  [layers]
  (vec (mapcat layer-input-partial layers)))

(defn- layer-filter-complex-partial
  [nmax [{px :x py :y pz :z :as pl} {cx :x cy :y n :z :as cl}]]
  (cond
    (= n 0) ""
    (and (= n 1) (= n nmax)) (str "[0:v][1:v]overlay=" cx ":" cy "")
    (= n 1) (str "[0:v][1:v]overlay=" cx ":" cy "[over001];")
    (< 1 n nmax) (str (format "[over%03d]" (dec n)) "[" n ":v]overlay=" cx ":" cy (format "[over%03d];" n))
    (= n nmax) (str (format "[over%03d]" (dec n)) "[" n ":v]overlay=" cx ":" cy)))

(defn- layer-filter-complex
  [layers]
  (let [nlayers (dec (count layers))
        layers (sort-by :z layers)]
    ["-filter_complex"
     (str/join " " (map (partial layer-filter-complex-partial nlayers) (partition 2 1 layers)))]))

(defn layer
  [fname layers]
  (apply sh (concat
             ["ffmpeg"]
             (layer-input layers)
             (layer-filter-complex layers)
             ["-c:a" "copy" "-map" "0:a:0"]
             ["-y" fname])))

(def example-layers 
  [{:file "clip001-bb.mov" :x 0 :y 0 :z 0}
   {:file "drw2.webm" :x 0 :y (- 1080 600) :z 1}])



(defn pad-screen
  [{:keys [fname left right] :as m}]
  (let [[w h] (get-resolution fname)
        props (merge m {:border {:width 0 :color ""}
                        :base-dims [1920 1080]
                        :overlay-dims [(+ (:width left) (:width right)) h]})
        [ow oh] (overlay-offsets props)
        col (get-bg-color fname)]
    (sh "ffmpeg"
        "-i" fname
        "-f" "lavfi" 
        "-i" (str "color=" col ":s=1920x1080")
        "-filter_complex"
        (str "[1:v][0:v]overlay=" ow ":" oh ":shortest=1")
        "-c:a" "copy" "-y" "cropped-screen.mov")))

(defn crop-pad-screen
  "A multi-step transformation for screen recording footage.
  
  The following sequence of transforms are handled using ffmpeg's 'filter_complex':
   - crop and pad screen recording
   - cut screen footage into left side and right side
   - create a 1920x1080 image with the background color as the fill
   - stitch left and right side back together
   - overlay stitched screen recording onto the bg image with calculated offset values"
  [{:keys [fname left right] :as m}]
  (let [[w h] (get-resolution fname)
        props (merge m {:border {:width 0 :color ""}
                        :base-dims [1920 1080]
                        :overlay-dims [(+ (:width left) (:width right)) h]})
        [ow oh] (overlay-offsets props)
        col (get-bg-color fname)]
    (sh "ffmpeg"
        "-i" fname
        "-f" "lavfi" 
        "-i" (str "color=" col ":s=1920x1080")
        "-filter_complex"
        (str "[0:v]crop=" (:width left) ":" h ":" (:offset left) ":0[l];"
             "[0:v]crop=" (:width right) ":" h ":" (- w (:width right) (:offset right)) ":0[r];"
             "[l][r]hstack=inputs=2[scr];"
             "[1:v][scr]overlay=" ow ":" oh ":shortest=1")
        "-c:a" "copy" "-y" "cropped-screen.mov")))

(defn clap-time
  "Find time in seconds at which a clap is detected in the audio stream of fname.
  
  The detection assumes that a clap sound exists within the first 12 seconds of a given clip."
  [fname]
  (->> (sh "ffmpeg" "-i" fname
           "-ss" "00:00:00" "-t" "00:00:12" 
           "-af" "silencedetect=noise=0.6:d=0.01"
           "-f" "null" "-")
       :err
       (str/split-lines)
       (drop-while #(not (str/includes? % "silence_end:")))
       (first)
       (re-find #"silence_end: .+")
       (re-find #"\d+\.\d+")
       (read-string)))

(defn overlay-camera
  "Composes the final footage by overlaying the camera footage onto the screen footage according to given properties.
  
  The composition is handled using ffmpeg's 'filter_complex', and several actions occur:
   - overlays camera footage with border onto screen footage
   - given screen footage, camera footage, and border width and color create combined video
   - calculate camera delay using clap times in footage. assumes screen recording is longer than cam
   - calculate size of border for camera
   - create border as a solid color frame
   - scale camera down to given overlay-dims
   - overlay camera onto border frame
   - overlay bordered camera onto screen footage with calculated offsets"
  [{:keys [border overlay-dims camf scrf] :as props}]
  (let [{:keys [width color]} border
        [cw ch] (map #(+ (* 2 width) %) overlay-dims)
        [ow oh] (overlay-offsets (assoc props :fname scrf 
                                        :base-dims (get-resolution scrf)))
        delay (- (clap-time scrf) (clap-time camf))]
    (sh "ffmpeg"
        "-i" scrf
        "-i" camf
        "-f" "lavfi" 
        "-i" (str "color=" color ":s=" cw "x" ch)
        "-filter_complex"
        (str "[1:v]scale=" (apply str (interpose "x" overlay-dims)) "[scv];"
             "[2:v][scv]overlay=" width ":" width ":shortest=1[cam];"
             "[cam]setpts=PTS-STARTPTS+" delay "/TB[dcam];"
             "[0:v][dcam]overlay=" ow ":" oh ":shortest=1")
        "-c:a" "copy" "-y" "merged.mov")))

(defn fix-audio
  "Fixes issue where mono audio track plays only to the Left channel."
  [fname]
  (sh "ffmpeg" "-i" fname
      "-i" fname "-af" "pan=mono|c0=FL"
      "-c:v" "copy" "-map" "0:v:0" "-map" "1:a:0" "fixed-audio.mov"))

(defn blur-border-resize
  [fname]
  (let [[name ext] (str/split fname #"\.")
        nfname (apply str [name "-bbr" "." ext])]
    (sh "ffmpeg"
        "-i" fname
        "-filter_complex"
        (str "[0:v]scale=1920:-1[sc1];"
             "[0:v]scale=1280:-1[sc2];"
             "[sc1]gblur=sigma=75[blur];"
             "[blur][sc2]overlay=(main_w-overlay_w)/2:(main_h-overlay_h)/2:shortest=1")
        "-c:a" "copy" "-y" nfname)))

(defn blur-border
  [fname backw backh forew foreh]
  (let [[name ext] (str/split fname #"\.")
        nfname (apply str [name "-bb" "." ext])]
    (sh "ffmpeg"
        "-i" fname
        "-filter_complex"
        (str "[0:v]scale=" backw ":" backh "[sc1];"
             "[0:v]scale=" forew ":" foreh "[sc2];"
             "[sc1]gblur=sigma=75[blur];"
             "[blur][sc2]overlay=(main_w-overlay_w)/2:(main_h-overlay_h)/2:shortest=1")
        "-c:a" "copy" "-y" nfname)))

(defn blur-uneven-border
  [fname backw backh forew foreh x y]
  (let [[name ext] (str/split fname #"\.")
        nfname (apply str [name "-bub" "." ext])]
    (sh "ffmpeg"
        "-i" fname
        "-filter_complex"
        (str "[0:v]scale=" backw ":" backh "[sc1];"
             "[0:v]scale=" forew ":" foreh "[sc2];"
             "[sc1]gblur=sigma=75[blur];"
             "[blur][sc2]overlay=" x ":" y ":shortest=1")
        "-c:a" "copy" "-y" nfname)))

(defn blur-border2
  [fnameb backw backh fnamef forew foreh]
  (let [[name ext] (str/split fnamef #"\.")
        nfname (apply str [name "-bb2" "." ext])]
    (sh "ffmpeg"
        "-i" fnameb
        "-i" fnamef
        "-filter_complex"
        (str "[0:v]scale=" backw ":" backh "[sc1];"
             "[1:v]scale=" forew ":" foreh "[sc2];"
             "[sc1]gblur=sigma=75[blur];"
             "[blur][sc2]overlay=(main_w-overlay_w)/2:(main_h-overlay_h)/2:shortest=1")
        "-c:a" "copy" "-map" "1:a:0"
        "-y" nfname)))

(defn corner-col-border
  [fname backw backh forew foreh]
  (let [[name ext] (str/split fname #"\.")
        nfname (apply str [name "-ccb" "." ext])]
    (sh "ffmpeg"
        "-i" fname
        "-i" fname
        "-filter_complex"
        (str "[0:v]crop=2:2[crop1];"
             "[crop1]scale=" (max backw backh) ":" (max backw backh) "[sc1];"
             "[sc1]crop=" backw ":" backh "[crop2];"
             "[1:v]scale=" forew ":" foreh "[sc2];"
             "[crop2][sc2]overlay=(main_w-overlay_w)/2:(main_h-overlay_h)/2:shortest=1")
        "-c:a" "copy" "-map" "0:a:0"
        "-y" nfname)))

(defn corner-col-uneven-border
  [fname backw backh forew foreh x y]
  (let [[name ext] (str/split fname #"\.")
        nfname (apply str [name "-ccub" "." ext])]
    (sh "ffmpeg"
        "-i" fname
        "-i" fname
        "-filter_complex"
        (str "[0:v]crop=2:2[crop1];"
             "[crop1]scale=" (max backw backh) ":" (max backw backh) "[sc1];"
             "[sc1]crop=" backw ":" backh "[crop2];"
             "[1:v]scale=" forew ":" foreh "[sc2];"
             "[crop2][sc2]overlay=" x ":" y ":shortest=1")
        "-c:a" "copy" "-map" "0:a:0"
        "-y" nfname)))

(defn static-bg
  [fnameb backw backh fnamef forew foreh]
  (let [[name ext] (str/split fnamef #"\.")
        nfname (apply str [name "-sb" "." ext])]
    (sh "ffmpeg"
        "-i" fnameb
        "-i" fnamef
        "-filter_complex"
        (str "[0:v]scale=" backw ":" backh "[sc1];"
             "[1:v]scale=" forew ":" foreh "[sc2];"
             "[sc1][sc2]overlay=(main_w-overlay_w)/2:(main_h-overlay_h)/2")
        "-c:a" "copy" "-map" "1:a:0"
        "-y" nfname)))

(defn nicer-vertical
  [fname]
  (let [[name ext] (str/split fname #"\.")
        nfname (apply str [name "-nice" "." ext])]
    (sh "ffmpeg"
        "-i" fname
        "-filter_complex"
        (str "[0:v]scale=w=in_h:h=-1[sc1];"
             "[sc1]crop=in_w:1080[crop];"
             "[crop]gblur=sigma=85[blur];"
             "[0:v]scale=w=-1:h=in_w[sc2];"
             "[blur][sc2]overlay=(main_w-overlay_w)/2:0:shortest=1")
        "-map" "0:a" "-c:a" "copy" "-y" nfname)))

(defn get-sounds
  [fname]
  (->> (sh "ffmpeg" "-i" fname
           "-af" "silencedetect=n=-37dB:d=0.7"
           "-f" "null" "-")
       :err
       (str/split-lines)
       (map #(re-find #"silence_.+" %))
       (filter #(not (nil? %)))
       (map #(re-find #"\d+\.?(\d+)?" %))
       (map first)
       (map read-string)
       (rest)
       (partition 2)
       (map #(zipmap [:s :e] %))))

(defn clip-video
  [fname {:keys [s e]}]
  (let [dur (- e s)
        tmpf (str (str/replace (str s) "." "_")
                  "-" fname)]
    (sh "ffmpeg" "-i" fname 
        "-ss" (str s) "-t" (str dur)
        tmpf)))

(defn cut-merge
  [fname times]
  (let [fnames (map 
                #(str (str/replace (str (:s %)) "." "_")
                      "-" fname)
                times)]
    ;; create clips
    (doall (pmap #(clip-video fname %) times))
    ;; create clips file list
    (spit (str fname ".txt")
          (apply str (map #(str "file '" % "'\n") fnames)))
    ;; concat clips together
    (sh "ffmpeg" "-f" "concat" "-safe" "0"
        "-i" (str fname ".txt") "-c" "copy"
        (str "initial-cut" (get-extension fname)))
    ;; delete tmp files
    (mapv #(sh "rm" %) fnames)
    (sh "rm" (str fname ".txt"))))

(def font-import
  [:style "
@import url('https://fonts.googleapis.com/css2?family=Oswald:wght@600&display=swap');

"])


(defn iso-text [text]
  (->> (svg/text text)
       (svg/style {:transform "rotate(0 0 0) matrix(0.707 0.409 -0.707 0.409 0 -0.816)"})))

(defn svg
  [[w h sc] & content]
  (assoc-in 
   (svg/svg [w h sc] 
            font-import
            content)
   [1 :viewBox]
   (str/join " " [(/ w -2.0) (/ h -2.0) w h])))

(def test-overlay
  (let [obj 
        (fn [t]
          (->> (iso-text "adam-james")
               (svg/style {:fill (str "rgb(100,170,123)")
                           :stroke (str "rgb(80,210,145)")
                           :opacity t
                           :stroke-width "1px"
                           :text-anchor "middle"
                           :font-size 120
                           :font-family "Oswald"
                           :font-weight "600"})))]
    (svg [600 600]
         (let [step 0.1]
           (for [t (range step (+ step 1) step)]
             (->> (obj t)
                  (tf/translate [(* -100 t) (* -100 t)])))))))

(defn iso-title
  [s]
  (let [obj 
        (fn [t]
          (->> (iso-text s)
               (svg/style {:fill (str "rgb(100,170,123)")
                           :stroke (str "rgb(80,210,145)")
                           :opacity t
                           :stroke-width "1px"
                           :text-anchor "middle"
                           :font-size 120
                           :font-family "Oswald"
                           :font-weight "600"})))]
    (svg [600 600]
         (let [step 0.1]
           (for [t (range step (+ step 1) step)]
             (->> (obj t)
                  (tf/translate [(* -100 t) (* -100 t)])))))))

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
            "-c:v" "libvpx-vp9" "-vf" "format=rgba"
            "-pix_fmt" "yuva420p" "-b:v" "800k"
            "-y" (str name ".webm"))]
    (when (= 0 (:exit ffmpeg))
      (sh "rm" "-rf" name))))

(defn animate! [{:keys [graphics-fn name framerate duration]}]
  (do (anim-frames! graphics-fn name framerate duration)
      (anim-video! name framerate)))

(defn ease-in-out-cubic [t]
  (if (< t 0.5)
    (* 4 t t t)
    (- 1 (/ (Math/pow (+ 2 (* t -2)) 3) 2))))

(def circle-anim
  {:name "circle"
   :framerate 30
   :duration 4
   :graphics-fn
   (fn [t]
     (let [nt (ease-in-out-cubic t)]
       (svg [600 600]
            (->> (svg/circle 35)
                 (tf/translate [-300 -300])
                 (tf/translate [(* nt 600) (* nt 600)])
                 (svg/style {:fill "pink"
                             :stroke "hotpink"
                             :stroke-width "4px"})))))})

(def draw-anim
  {:name "drw"
   :framerate 60
   :duration 4
   :graphics-fn
   (fn [t]
     (let [nt (ease-in-out-cubic t)]
       (svg
        [600 600]
        (->> (svg/rect 600 600)
             (svg/style {:fill "rgb(45,52,64)"}))
        (->> (iso-text "adam-james")
             (svg/style {:fill "none"
                         :stroke-dasharray 600
                         :stroke-dashoffset (* 600 (- 1 nt))
                         :text-anchor "middle"
                         :stroke-width "1px"
                         :stroke "hotpink"
                         :font-family "Oswald"
                         :font-weight "600"
                         :font-size "120"})))))})

(def palette ["#5E81AC"
              "#81a1c1"
              "#8fbcbb"
              "#bf616a"
              "#d08770"
              "#ebcb8b"
              "#a3be8c"
              "#b48ead"
              "#88c0d0"])

(defn draw-title
  [text]
  (let [col (get palette (rand-int (count palette)))]
    (fn [t]
      (let [nt (ease-in-out-cubic t)]
        (svg
         [600 600 1]
         (->> (iso-text text)
              (tf/translate [5 5])
              (svg/style {:fill "none"
                          :opacity 0.15
                          :stroke-dasharray 600
                          :stroke-dashoffset (* 600 (- 1 nt))
                          :text-anchor "middle"
                          :stroke-width "2px"
                          :stroke "#eceff4"
                          :font-family "Oswald"
                          :font-weight "600"
                          :font-size "100"}))
         (->> (iso-text text)
              (tf/translate [2.5 2.5])
              (svg/style {:fill "none"
                          :opacity 0.5
                          :stroke-dasharray 600
                          :stroke-dashoffset (* 600 (- 1 nt))
                          :text-anchor "middle"
                          :stroke-width "2px"
                          :stroke "#eceff4"
                          :font-family "Oswald"
                          :font-weight "600"
                          :font-size "100"}))
         (->> (iso-text text)
              (svg/style {:fill "none"
                          :stroke-dasharray 600
                          :stroke-dashoffset (* 600 (- 1 nt))
                          :text-anchor "middle"
                          :stroke-width "2px"
                          :stroke col
                          :font-family "Oswald"
                          :font-weight "600"
                          :font-size "100"})))))))

(defn draw-text
  [text]
  (let [col (get palette (rand-int (count palette)))]
    (fn [t]
      (let [nt (ease-in-out-cubic t)]
        (svg
         [900 300 1]
         (->> (svg/text text)
              (tf/translate [0 2])
              (svg/style {:fill "none"
                          :opacity 0.15
                          :stroke-dasharray 600
                          :stroke-dashoffset (* 600 (- 1 nt))
                          :text-anchor "middle"
                          :stroke-width "2px"
                          :stroke "#eceff4"
                          :font-family "Menlo"
                          :font-size "29"}))
         (->> (svg/text text)
              (tf/translate [0 1])
              (svg/style {:fill "none"
                          :opacity 0.5
                          :stroke-dasharray 600
                          :stroke-dashoffset (* 600 (- 1 nt))
                          :text-anchor "middle"
                          :stroke-width "2px"
                          :stroke "#eceff4"
                          :font-family "Menlo"
                          :font-size "29"}))
         (->> (svg/text text)
              (svg/style {:fill "none"
                          :stroke-dasharray 600
                          :stroke-dashoffset (* 600 (- 1 nt))
                          :text-anchor "middle"
                          :stroke-width "2px"
                          :stroke col
                          :font-family "Menlo"
                          :font-size "29"})))))))

(def draw-anim2
  {:name "drw2"
   :framerate 30
   :duration 4
   :graphics-fn (draw-text "This is a short sentence explaining the clip.")})


(comment
  (animate! circle-anim)
  (animate! circle2-anim)
  ;; WIP
  (layer "circles.webm"
         [{:file "circle.webm" :x 0 :y 0 :t 0}
          {:file "circle2.webm" :x 0 :y 0 :t 0}])
  )

(def title-anim
  {:name "twitch"
   :framerate 60
   :duration 5
   :graphics-fn
   (fn [t]
     (let [nt (ease-in-out-cubic t)]
       (svg/svg 
        [1920 1080 1]
        #_(->> (svg/circle (* 1200 2 nt))
             (tf/translate [960 540])
             (svg/style {:fill "rgb(45,52,64)"}))
        (->> (svg/text "twitch.tv/adam_james_tv")
             (tf/rotate 0)
             (tf/translate [960 540])
             (tf/translate [0 450])
             (svg/style {:fill "#FEFEFE"
                         :opacity "0.90"
                         :text-anchor "middle"
                         :font-family "Oswald"
                         :font-weight "600"
                         :font-size "120px"}))
        (->> (svg/text "twitch.tv/adam_james_tv")
             (tf/rotate 0)
             (tf/translate [960 540])
             (tf/translate [0 450])
             (svg/style {:fill "#9146FF"
                         :opacity "0.25"
                         :text-anchor "middle"
                         :font-family "Oswald"
                         :font-weight "600"
                         :font-size "120px"}))
        (->> (svg/text "twitch.tv/adam_james_tv")
             (tf/rotate 0)
             (tf/translate [960 540])
             (tf/translate [0 450])
             (svg/style {:fill "none"
                         :stroke-dasharray 700
                         :stroke-dashoffset (* 700 (- 1 nt))
                         :stroke-width "2px"
                         :stroke "#9146FF"
                         :text-anchor "middle"
                         :font-family "Oswald"
                         :font-weight "600"
                         :font-size "120px"})))))})

;; input
(def sample-clip
  {:fname "sample.mov"
   :url "https://youtu.be/NDo0fmRshrM?t=6663"
   :duration 40
   :title "OpenSCAD Basics"
   :descr "Showing what union() and difference() do"})

;; messy, but it works!!
(defn stream-highlight-clip!
  [{:keys [fname url duration title descr] :as clipm}]
  (let [[name ext] (str/split fname #"\.")
        anim-name (str name "/anim")
        anim2-name (str name "/anim2")
        base-clip-name (str name "/base-clip")
        xf-clip-name (str name "/base-clip-bub.mov")]
    ;; create clip directory
    (sh "mkdir" "-pv" (str name))
    ;; create all animation layers
    (animate! {:name anim-name 
               :duration 5
               :framerate 30
               :graphics-fn (draw-title title)})
    (animate! {:name anim2-name
               :duration 3
               :framerate 30
               :graphics-fn (draw-text descr)})
    ;; retrieve the base clip
    (clip! base-clip-name url duration)
    ;; clip 'effect'
    (blur-uneven-border (str base-clip-name ".mov") 1920 1080 1600 900 300 20)
    ;; compose all layers
    (layer fname 
           [{:file xf-clip-name :x 0 :y 0 :z 0}
            {:file (str anim-name ".webm") :x -30 :y (- 1080 500) :z 1}
            {:file (str anim2-name ".webm") :x (- 1920 1000) :y (- 1080 250) :z 2}])
    ;; can delete intermediate files, but probably good to keep them
    #_(sh "rm" "-rf" (str name))))

(defn crossfade
  [[fnamea fnameb] dur]
  (let [[namea exta] (str/split fnamea #"\.")
        [nameb extb] (str/split fnameb #"\.")
        nameb (last (str/split nameb #"/"))
        fname (str namea "-" nameb "." exta)
        dura (get-duration fnamea)
        durb (get-duration fnameb)
        [w h] (get-resolution fnamea)
        total-dur (+ dura durb (- dur))]
    (sh "ffmpeg"
        "-i" fnamea
        "-i" fnameb
        "-filter_complex"
        (str "color=black:1920x1080:d=" total-dur "[base];"
             "[0:v]setpts=PTS-STARTPTS[v0];"
             "[1:v]format=yuva420p,fade=in:st=0:d=" dur ":alpha=1,setpts=PTS-STARTPTS+(" (- dura dur) "/TB)[v1];"
             "[base][v0]overlay[tmp];"
             "[tmp][v1]overlay,format=yuv420p[fv];"
             "[0:a][1:a]acrossfade=d=" dur "[fa]")
        "-map" "[fv]" "-map" "[fa]"
        "-y" fname)))

(def example-props
    {:screen
     {:fname "scr.mov"
      :left {:width 667 :offset 0}
      :right {:width 750 :offset 0}
      :gap 100
      :pos {:h :l :v :c}}

     :camera
     {:camf "cam.mov"
      :scrf "cropped-screen.mov"
      :border {:width 7 :color "cyan"}
      :overlay-dims [480 270]
      :gap 70
      :pos {:h :r :v :b}}})

(defn stream-highlighter
  []
  (let [fname (first *command-line-args*)
        clips (when (= (get-extension fname) ".edn")
                (read-string (slurp fname)))]
    (do
      (println (str "Total Seconds: " (reduce + (map :duration clips))))
      (mapv stream-highlight-clip! clips)
      #_(println "done"))))

(defn main
  "Main runs when vidwiz is run as a script.
  
  You can run this program with babashka:
   - chmod +x vidwiz.clj
   - ./vidwiz props.edn"
  []
  (let [fname (first *command-line-args*)
        props (when (= (get-extension fname) ".edn")
                (read-string (slurp fname)))]
    (when props
      (crop-pad-screen (:screen props))
      (pad-screen (:screen props))
      (overlay-camera  (:camera props))
      (fix-audio "merged.mov")
      (sh "cp" "fixed-audio.mov" "_precut.mov"))))

#_(stream-highlighter)
