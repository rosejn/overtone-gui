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
         slide-tx (sg/translate slide-scale 1 handle-height)
         value (atom 0.8)
         last-y (atom 0)]
     (doto box
       (sg/anti-alias :on)
       (sg/mode :stroke)
       (sg/stroke-color (get-color :stroke-1))
       (sg/set-shape (sg/round-rectangle 0 0 FADER-WIDTH FADER-HEIGHT
                                            FADER-CORNER-FADER-WIDTH
                                            FADER-CORNER-FADER-HEIGHT)))

     (doto slide
       (sg/anti-alias :on)
       (sg/mode :fill)
       (sg/fill-color (get-color :fill-1))
       (sg/set-shape (sg/round-rectangle 0 0
                                            FADER-WIDTH
                                            FADER-HEIGHT
                                            FADER-CORNER-FADER-WIDTH
                                            FADER-CORNER-FADER-HEIGHT)))
     (doto handle
       (sg/anti-alias :on)
       (sg/mode :fill)
       (sg/fill-color (get-color :stroke-1))
       (sg/set-shape (sg/round-rectangle 0 0
                                            FADER-WIDTH
                                            handle-height
                                            (/ FADER-CORNER-FADER-WIDTH 2)
                                            (/ FADER-CORNER-FADER-HEIGHT 2))))

     (add-watch value (gensym "fader")
        (fn [_ _ _ new-val]
          (let [y  (- FADER-HEIGHT (* new-val FADER-HEIGHT))
                hy (- (- FADER-HEIGHT handle-height)
                      (* new-val (- FADER-HEIGHT handle-height))) ]
            (.setTranslateY handle-tx hy)
            (.setTranslateY slide-tx y)
            (.setScaleY slide-scale new-val)
            )))

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
       (sg/on-mouse-pressed handle press-handler)
       (sg/on-mouse-pressed slide press-handler)
       (sg/on-mouse-dragged handle drag-handler)
       (sg/on-mouse-dragged slide drag-handler))

     (sg/add group slide-tx handle-tx box)

     {:type :fader
      :group group
      :value value})))
