(ns mandelbrotter.core
  (:use clojure.contrib.profile)
  (:require clojure.contrib.seq)
  (:import (java.awt Color
                     Dimension)
           (java.awt.event MouseAdapter)
           (java.awt.image BufferedImage)
           (javax.swing JFrame
                        JPanel)))

;; 座標系は左上を原点として、
;; 右にx、下にyが伸びる

;;
;; データは一次元のシーケンスで、
;; 座標 [x y] の値は (+ x (* size-y y)) 番目に存在する
(defrecord Pixel [value z divergence? times])

(defn- initialize-data
  [[center-x center-y] [scope-x scope-y] [size-x size-y]]
  (let [origin-x (- center-x (/ scope-x 2))
        origin-y (- center-y (/ scope-y 2))
        xs (map #(+ (/ (* % scope-x) size-x) origin-x) (range size-x))
        ys (map #(+ (/ (* % scope-y) size-y) origin-y) (reverse (range size-y)))]
    (for [y ys
          x xs]
      (Pixel. [x y] [0 0] false 0))))

(defn create-mandelbrot-set
  [& {:keys [center scope size]
      :or {center [0.7 0] scope [3 3] size [400 400]}}]
  (assoc {:center center :scope scope :size size :times 0}
    :data (vec (initialize-data center scope size))))

(defn cabs
  [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn divergence?
  [c]
  (< 2 (cabs c)))

(defn next-z
  [[a b] [x y]]
  (let [xn (+ (- (* x x) (* y y)) a)
        yn (+ (* 2 x y) b)]
    [xn yn]))

(defn next-pixel
  [p]
  (if (:divergence? p)
    p
    (let [{c :value z :z t :times} p
          zn (next-z c z)]
      (Pixel. c zn (divergence? zn) (+ t 1)))))

(defn next-mandelbrot-set
  [ms]
  (-> ms
      (assoc :data (vec (map next-pixel (:data ms))))
      (assoc :times (+ 1 (:times ms)))))

(defn hsl->rgb
  "Convert [H S L] to [R G B]
   H [0 1)
   S [0 1]
   L [0 1]
   R [0 1]
   G [0 1]
   B [0 1]"
  [h s l]
  (if (= s 0)
    [l l l]
    (let [m2 (if (<= l 0.5)
               (* l (+ 1 s))
               (+ (* l (- 1 s)) s))
          m1 (- (* 2 l) m2)
          h-prime (mod (* h 6) 6)
          v1 (+ m1 (* (- m2 m1) h-prime))
          v2 (+ m1 (* (- m2 m1) (- 4 h-prime)))]
      (condp = (int h-prime)
          0 [m2 v1 m1]
          1 [v2 m2 m1]
          2 [m1 m2 v1]
          3 [m1 v2 m2]
          4 [v1 m1 m2]
          5 [m2 m1 v2]))))

(defn hsl->rgbint
  [h s l]
  (let [[#^Float r #^Float g #^Float b] (hsl->rgb h s l)]
    (+ (bit-shift-left (int (* r 255)) 16)
       (bit-shift-left (int (* g 255)) 8)
       (bit-shift-left (int (* b 255)) 0))))

(defmacro px
  [& xs]
  `(try
     ~@xs
     (catch Exception e#
       (prn e#)
       (throw e#))))

(defn pixel->rgbint
  [p]
  (let [n (/ (- (:times p) (Math/log (Math/log (cabs (:z p)))))
             (Math/log 2))
        max 50
        c (mod n max)]
    (if (<= c (/ max 2))
      (hsl->rgbint (/ 17 255) 1 (/ c (/ max 2)))
      (hsl->rgbint (/ 145 255) 1 (/ (- max c) (/ max 2))))))

(defn mandelbrot->image [m]
  (let [[x y] (:size m)
        image (BufferedImage. x y BufferedImage/TYPE_INT_RGB)
        buffer (-> image .getRaster .getDataBuffer)]
    (doseq [[i p] (clojure.contrib.seq/indexed (:data m))]
      (if (= (:times m) (:times p))
        (.setElem buffer i 0)
        (.setElem buffer i (pixel->rgbint p))))
    image))

(def *mandelbrot* (ref nil))

(defn initialize-mandelbrot
  [& ms]
  (dosync
   (ref-set *mandelbrot*
            (apply create-mandelbrot-set ms)))
  nil)

(defn update-mandelbrot! [mandelbrot times]
  (dotimes [n times]
    (dosync
     (alter mandelbrot next-mandelbrot-set)))
  mandelbrot)

(defn main-panel
  [image]
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (if @image
        (time (.drawImage g @image 0 0 this))))))

(defn create-frame
  [x y]
  (let [image (ref nil)
        frame (JFrame. "mandelbrot")
        panel (main-panel image)]
    (doto panel
      (.setPreferredSize (Dimension. x y)))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    [panel image]))

(defn -main
  [x y]
  (let [[panel image] (create-frame x y)
        mandelbrot (ref (create-mandelbrot-set :center [-0.87591 0.20464]
                                               :scope [0.5 0.5]
                                               :size [x y]))]
    (future (update-mandelbrot! mandelbrot 100))
    (.addMouseListener panel
                       (proxy [MouseAdapter] []
                         (mouseClicked [e]
                           (dosync (ref-set image (mandelbrot->image @mandelbrot)))
                           (.repaint panel))))
    ))
