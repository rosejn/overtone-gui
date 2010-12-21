(ns
  ^{:doc "A surface label"
    :author "Jeff Rose"}
  overtone.gui.surface.label
  (:use
    [overtone event]
    [overtone.gui color])
  (:require [overtone.gui.sg :as sg]))

(def LABEL-PAD 4)

(defn label-group
  [{:keys [color]}]
  (let [group (sg/group)
        box (sg/shape)
        txt (sg/text "label")
        {:keys [width height]} (bean (.getBounds txt))
        value (atom "Label")
        border? (atom false)
        background? (atom false)]
    (doto box
      (sg/mode :fill)
      (sg/fill-color (overtone.gui.color/color 0 0 0 0)))

    (doto txt
      (sg/set-font "SansSerif" 12 :bold)
      (sg/mode :fill)
      (sg/fill-color (get-color :text)))

    (sg/observe value 
      (fn [new-txt]
        (sg/set-text txt new-txt)
        (let [{:keys [width height]} (bean (.getBounds txt))]
          (sg/set-shape box (sg/round-rectangle 0 0 
             (+ width (* 2 LABEL-PAD) 1)
             (+ height (* 2 LABEL-PAD))
             (* 2 LABEL-PAD) (* 2 LABEL-PAD))))))

    (sg/observe border?
      (fn [new-val]
        (doto box
          (sg/mode (if @background? :stroke-fill :stroke))
          (sg/stroke-color (or color (sg/color 255 255 255))))))

    (sg/observe background?
      (fn [new-val]
        (doto box
          (sg/mode (if @border? :stroke-fill :fill))
          (sg/fill-color (or color (sg/color 255 255 255))))))

    (sg/add group box (sg/translate txt (+ 2 LABEL-PAD)
                                    (+ height LABEL-PAD)))
    
    {:type :label
     :group group
     :color color
     :value value
     :border? border?
     :background? background?}))
