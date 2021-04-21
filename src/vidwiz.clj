#!/usr/bin/env bb

(ns vidwiz.main
  "This is a prototype script for automating a portion of my video editing using ffmpeg."
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

;; utils
(defn get-extension
  [fname]
  (re-find #"\.[A-Za-z\d+]+" fname))

(defn get-resolution
  [fname]
  (->> (sh "ffprobe" "-i" fname)
       :err
       (re-find #"\d\d+x\d+")
       (#(str/split % #"x"))
       (mapv read-string)))

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
                   (st/split-lines)
                   (drop-while #(not (st/includes? % "Histogram")))
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
  (let [[url & rest] (str/split url #"[\?&]")
        xr (map #(str/split % #"=") rest)
        keys (map keyword (into [:url] (map first xr)))
        vals (into [url] (map second xr))]
    (zipmap keys vals)))

(defn yt-url->video-data [url]
  (let [[title video-url _ descr]
        (-> (sh "youtube-dl" "-e" "-g" "--get-description" url)
            (:out)
            (str/split-lines))]
    {:title title
     :descr descr
     :video-url video-url}))

(defn save-clip! [video-url time dur fname]
  (sh "ffmpeg" 
      "-ss" time
      "-i" video-url
      "-t" dur
      "-s" "1920x1080" 
      fname))

(defn clip! [url dur]
  (let [urlp (parse-url url)
        data (yt-url->video-data (:url urlp))
        video-url (:video-url data)
        name (clean-name (:title data))
        stime (seconds->timestamp (read-string (:t urlp)))
        fname (str name ".mov")]
    (save-clip! video-url stime dur fname)))

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
       (st/split-lines)
       (drop-while #(not (st/includes? % "silence_end:")))
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

(defn nicer-vertical
  [fname]
  (let [[name ext] (st/split "." fname)
        nfname (apply str [name "-nice" "." ext])]
    (sh "ffmpeg"
        "-i" fname
        "-filter_complex"
        (str "[0:v]scale=w=ih[sc1];"
             "[sc1]crop=h=1080[crop]"
             "[crop]gblur=steps=20[blur]"
             "[0:v]scale=h=iw[sc2];"
             "[blur][sc2]overlay=(main_w-overlay_w)/2:0:shortest=1")
        "-c:a" "copy" "-y" nfname)))

(defn get-sounds
  [fname]
  (->> (sh "ffmpeg" "-i" fname
           "-af" "silencedetect=n=-37dB:d=0.7"
           "-f" "null" "-")
       :err
       (st/split-lines)
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
        tmpf (str (st/replace (str s) "." "_")
                  "-" fname)]
    (sh "ffmpeg" "-i" fname 
        "-ss" (str s) "-t" (str dur)
        tmpf)))

(defn cut-merge
  [fname times]
  (let [fnames (map 
                #(str (st/replace (str (:s %)) "." "_")
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

#_(spit "props.edn" 
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
      #_(crop-pad-screen (:screen props))
      (pad-screen (:screen props))
      (overlay-camera  (:camera props))
      (fix-audio "merged.mov")
      (sh "cp" "fixed-audio.mov" "_precut.mov"))))

(main)
