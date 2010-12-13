(ns mandelbrotter.core
  (:use clojure.contrib.profile)
  (:require clojure.contrib.seq)
  (:import (java.awt Color
                     Dimension)
           (java.awt.event MouseListener)
           (javax.swing JFrame
                        JPanel)))

;; 座標系はOpenGL方式で、
;; 左下を原点として、右にx、上にyが伸びる

;;
;; データは一次元のシーケンスで、
;; 座標 [x y] の値は (+ x (* size-y y)) 番目に存在する
(defrecord Pixel [value z divergence? times])

(defn- initialize-data
  [[center-x center-y] [scope-x scope-y] [size-x size-y]]
  (let [origin-x (- center-x (/ scope-x 2))
        origin-y (- center-y (/ scope-y 2))
        xs (map #(+ (/ (* % scope-x) size-x) origin-x) (range size-x))
        ys (map #(+ (/ (* % scope-y) size-y) origin-y) (range size-y))]
    (for [y ys
          x xs]
      (Pixel. [x y] [0 0] false 0))))

(defn create-mandelbrot-set
  [& {:keys [center scope size]
      :or {center [0.7 0] scope [3 3] size [400 400]}}]
  (assoc {:center center :scope scope :size size}
    :data (vec (initialize-data center scope size))))

(defn cabs
  [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn divergence?
  [c]
  (< 2 (cabs c)))

(defn next-z
  [[a b] [x y]]
  (let [xn (+ (- (* x x) (* y y) a))
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
  (assoc ms :data (vec (map next-pixel (:data ms)))))

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

(defn hsl->color
  [h s l]
  (let [[#^Float r #^Float g #^Float b] (hsl->rgb h s l)]
    (Color. r g b)))

(defmacro px
  [& xs]
  `(try
     ~@xs
     (catch Exception e#
       (prn e#)
       (throw e#))))

(defn pixel->color
  [p]
  (let [n (/ (- (:times p) (Math/log (Math/log (cabs (:z p)))))
             (Math/log 2))
        max 50
        c (mod n max)]
    (if (<= c (/ max 2))
      (hsl->color (/ 17 255) 1 (/ c (/ max 2)))
      (hsl->color (/ 145 255) 1 (/ (- max c) (/ max 2))))))

(def pixel->color (memoize pixel->color))

(defn paint [g mandelbrot]
  (println "*paint*")
  (let [[width height] (:size mandelbrot)
        data (:data mandelbrot)]
    (prn (count (filter :divergence? data)))
    (doseq [[i p] (clojure.contrib.seq/indexed data)]
      (try
        (let [x (- width (rem i width) 1)
              y (- height (quot i width) 1)]
          (prof :setColor (.setColor g (if (:divergence? p)
                                         (prof :pixel->color (pixel->color p))
                                         Color/BLACK)))
          (prof :fillRect (.fillRect g x y 1 1)))
        (catch Exception e)))
    ))

(def *mandelbrot* (ref nil))

(defn initialize-mandelbrot
  [& ms]
  (dosync
   (ref-set *mandelbrot*
            (apply create-mandelbrot-set ms)))
  nil)

(defn update-mandelbrot [times]
  (if @*mandelbrot*
    (dotimes [n times]
      (dosync
       (alter *mandelbrot* next-mandelbrot-set))
      (if (= 0 (mod (+ n 1) 10))
        (prn (+ n 1)))
      )))

(defn main-panel
  []
  (proxy [JPanel MouseListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (if @*mandelbrot*
        (time (profile (paint g @*mandelbrot*)))))
    (mousePressed [e])
    (mouseReleased [e])
    (mouseEntered [e])
    (mouseExited [e])
    (mouseClicked [e]
      (.repaint this))))

(defn -main
  []
  (let [frame (JFrame. "mandelbrot")
        panel (main-panel)]
    (doto panel
      (.setPreferredSize (Dimension. 400 400))
      (.addMouseListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    ))
