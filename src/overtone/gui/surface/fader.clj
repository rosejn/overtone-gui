(ns
  ^{:doc "A fader control"
    :author "Jeff Rose"}
  overtone.gui.surface.fader
  (:use
    [overtone event]
    [overtone.gui color])
  (:require [overtone.gui.sg :as sg]))

(def FADER-WIDTH 20)
(def FADER-HEIGHT 140)
(def FADER-CORNER-FADER-WIDTH 5)
(def FADER-CORNER-FADER-HEIGHT 5)

(defn fader
  ([] (fader false))
  ([handler]
   (let [group (sg/group)
         box (sg/shape)
         handle-height (/ FADER-HEIGHT 15)
         handle (sg/shape)
         handle-tx (sg/translate handle 0 0)
         slide (sg/shape)
         slide-scale (sg/scale slide 1 (/ (- FADER-HEIGHT handle-height) FADER-HEIGHT))
         slide-tx (sg/translate slide-scale 0 handle-height)
         value (atom 0.8)
         last-y (atom 0)
         f-color (atom (get-color :stroke-1))]
     (doto box
       (sg/anti-alias :on)
       (sg/mode :stroke-fill)
       (sg/stroke-color @f-color)
       (sg/fill-color (get-color :background))
       (sg/set-shape (sg/round-rectangle 0 0 FADER-WIDTH FADER-HEIGHT
                                            FADER-CORNER-FADER-WIDTH
                                            FADER-CORNER-FADER-HEIGHT)))

     (doto slide
       (sg/anti-alias :on)
       (sg/mode :fill)
       (sg/fill-color (transparent-color @f-color))
       (sg/set-shape (sg/round-rectangle 0 0
                                            FADER-WIDTH
                                            FADER-HEIGHT
                                            FADER-CORNER-FADER-WIDTH
                                            FADER-CORNER-FADER-HEIGHT)))
     (doto handle
       (sg/anti-alias :on)
       (sg/mode :fill)
       (sg/fill-color @f-color)
       (sg/set-shape (sg/round-rectangle 0 0
                                            FADER-WIDTH
                                            handle-height
                                            (/ FADER-CORNER-FADER-WIDTH 2)
                                            (/ FADER-CORNER-FADER-HEIGHT 2))))

     (sg/add group box slide-tx handle-tx)
;     (sg/block-mouse group true)

     (sg/observe f-color
       (fn [new-color]
         (sg/stroke-color box new-color)
         (sg/fill-color slide (transparent-color new-color))
         (sg/fill-color handle new-color)))

     (sg/observe value
        (fn [new-val]
          (let [y  (- FADER-HEIGHT (* new-val FADER-HEIGHT))
                hy (- (- FADER-HEIGHT handle-height)
                      (* new-val (- FADER-HEIGHT handle-height)))]
            (.setTranslateY handle-tx hy)
            (.setTranslateY slide-tx y)
            (.setScaleY slide-scale new-val))))

     (reset! value 0.8)

     (let [press-handler
           (fn [event]
             (let [y (.getY event)]
               (reset! last-y y)))

           drag-handler
           (fn [event]
             (let [cur-y (.getY event)
                   dy (- @last-y cur-y)
                   dv (if (.isShiftDown event)
                        (* 0.001 dy)
                        (* 0.01 dy))
                   val (max 0 (min (+ @value dv) 1))]
               (reset! last-y cur-y)
               (reset! value val)
               (if handler
                 (handler val))))]

       (sg/on-mouse group
         :press press-handler
         :drag  drag-handler))

     {:type :fader
      :group group
      :value value
      :color f-color})))
