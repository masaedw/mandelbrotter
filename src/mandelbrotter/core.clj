(ns mandelbrotter.core)

(defn main-
  []
  (let [frame (mandelbrotter.MainFrame.)]
    (doto frame
    ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))))
