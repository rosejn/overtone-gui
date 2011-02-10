(ns overtone.gui.surface.properties
  (:use overtone.event)
  (:require [overtone.gui.sg :as sg]
            [overtone.gui.swing :as swing])
  (:import [javax.swing JScrollPane JPanel JLabel BoxLayout]
           [java.awt Graphics Dimension Color BasicStroke Font   
            GridLayout]))

(defn surface-property-panel
  [props]
  (let [panel (JPanel.)
        name-lbl   (JLabel. "Name:")
        width-lbl  (JLabel. "Width:")
        height-lbl (JLabel. "Height:")]
    (doto panel
      (.add name-lbl)
      (.add width-lbl)
      (.add height-lbl))

    (sg/observe (:surface props)
      (fn [new-surf]))

    panel))

(defn widget-property-panel
  [props]
  (let [panel (JPanel.)
        name-lbl   (JLabel. "Name: ")
        x-lbl      (JLabel. "X:")
        y-lbl      (JLabel. "Y:")
        width-lbl  (JLabel. "Width: ")
        height-lbl (JLabel. "Height:")]
    (doto panel
      (.setLayout (GridLayout. 5 2))
      (.add name-lbl)
      (.add x-lbl)
      (.add y-lbl)
      (.add width-lbl)
      (.add height-lbl))

    (sg/observe (:widget props)
      (fn [new-widget]))

    panel))

(defn properties-frame
  []
  (let [frame (sg/frame "Properties" 250 500)
        base-panel (JPanel.)
        scroller (JScrollPane. base-panel)
        p-surf (atom nil)
        p-widget (atom nil)
        props {:frame frame
               :surface p-surf
               :widget p-widget}]
    (doto base-panel
      (.setLayout (BoxLayout. base-panel BoxLayout/Y_AXIS))
      (.add (surface-property-panel props))
      (.add (widget-property-panel props)))

    (on-event :overtone.gui.surface.core/widget-selected :prop-handler
              (fn [{:keys [surface widget]}]
                (reset! p-surf surface)
                (reset! p-widget widget)))

    (doto frame
      (.add scroller)
      (.setPreferredSize (Dimension. 250 500))
      (.pack)
      (.show))

    props))
