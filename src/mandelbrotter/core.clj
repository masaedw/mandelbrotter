(ns mandelbrotter.core
  (:import (java.awt Dimension)
           (javax.swing JPanel JFrame)
           (java.awt.event MouseListener)))

(defn paint [g mandelbrot]
  )

(defn main-panel
  [mandelbrot]
  (proxy [JPanel MouseListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (paint g @mandelbrot))
    (mousePressed [e])
    (mouseReleased [e])
    (mouseEntered [e])
    (mouseExited [e])
    (mouseClicked [e]
      (.repaint this))))

(defn create-mandelbrot
  []
  {})

(defn main-
  []
  (let [mandel (ref (create-mandelbrot))
        frame (JFrame. "mandelbrot")
        panel (main-panel mandel)]
    (doto panel
      (.setPreferredSize (Dimension. 800 600))
      (.addMouseListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    [mandel]))
