(ns mandelbrotter.MainFrame
  (:gen-class
   :post-init post-init
   :extends javax.swing.JFrame))

(defn -post-init
  [this]
  (.setTitle this "mandelbrot")
  (let [panel (mandelbrotter.MainPanel.)]
    (.add (.getContentPane this) panel))
  (.pack this))
