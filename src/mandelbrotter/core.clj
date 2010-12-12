(ns mandelbrotter.core
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
;; 座標 [x y] の値は (+ x (+ size-y y)) 番目に存在する
(defrecord Pixel [value z divergence? times])

(defn- initialize-data
  [[center-x center-y] [scope-x scope-y] [size-x size-y]]
  (let [origin-x (- center-x (/ scope-x 2))
        origin-y (- center-y (/ scope-y 2))
        xs (map #(+ (/ (* % scope-y) size-y) origin-y) (range size-y))
        ys (map #(+ (/ (* % scope-x) size-x) origin-x) (range size-x))]
    (for [y ys
          x xs]
      (Pixel. [x y] [0 0] false 0))))

(defn create-mandelbrot-set
  []
  (let [ms {:center [0 0.5]
            :scope [2.5 2.5]
            :size [400 400]}]
    (assoc ms :data (initialize-data (:center ms)
                                     (:scope ms)
                                     (:size ms)))))

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
  (letfn []
    (assoc ms :data (map next-pixel (:data ms)))))

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

(defn pixel->color
  [p]
  (let [n (/ (- (:times p) (Math/log (Math/log (cabs (:z p)))))
             (Math/log 2))
        c (mod n 512)]
    (if (<= c 255)
      (hsl->color (/ 17 255) 1 (/ c 255))
      (hsl->color (/ 145 255) 1 (/ (- 511 c) 255)))))

(defn paint [g mandelbrot]
  (let [[width height] (:size mandelbrot)
        data (:data mandelbrot)]
    (prn (count (filter :divergence? data)))
    (doseq [[i p] (clojure.contrib.seq/indexed data)]
      (doto g
        (.setColor (if (:divergence? p)
                     (pixel->color p)
                     Color/BLACK))
        (.fillRect (- width (rem i width))
                   (- height (quot i width))
                   1
                   1))
      )))

(defn main-panel
  [mandelbrot]
  (proxy [JPanel MouseListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (paint g @mandelbrot))
    (mousePressed [e]
      (println "pressed")
      (dosync
       (alter mandelbrot
              #(-> %
                   next-mandelbrot-set
                   next-mandelbrot-set
                   next-mandelbrot-set
                   next-mandelbrot-set
                   next-mandelbrot-set
                   next-mandelbrot-set
                   next-mandelbrot-set
                   next-mandelbrot-set
                   next-mandelbrot-set
                   next-mandelbrot-set
                   next-mandelbrot-set
                   next-mandelbrot-set
                   next-mandelbrot-set)))
      (.repaint this))
    (mouseReleased [e])
    (mouseEntered [e])
    (mouseExited [e])
    (mouseClicked [e]
      (.repaint this))))

(defn -main
  []
  (let [ms (ref (create-mandelbrot-set))
        frame (JFrame. "mandelbrot")
        panel (main-panel ms)]
    (doto panel
      (.setPreferredSize (Dimension. 400 400))
      (.addMouseListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    [ms]))
