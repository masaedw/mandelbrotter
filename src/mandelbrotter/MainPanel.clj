(ns mandelbrotter.MainPanel
  (:import java.awt.Dimension)
  (:gen-class :extends javax.swing.JPanel
              :post-init post-init
              :exposes-methods {paintComponent paintComponentSuper}))

(defn -post-init
  [this]
  (.setPreferredSize this (Dimension. 800 600)))

(defn -paintComponent
  [this g]
  (.paintComponentSuper this g))
