(ns
  ^{:doc "A simple button widget"
    :author "Jeff Rose"}
  overtone.gui.surface.button
  (:use
    [overtone event]
    [overtone.gui color])
  (:require [overtone.gui.sg :as sg]))

(def BUTTON-SIZE 20)
(def BUTTON-CORNER 5)

(defn button
  ([] (button false))
  ([handler]
   (let [group  (sg/group)
         back   (sg/shape)
         box    (sg/shape)
         x-pos  0
         y-pos  0
         status (atom false)
         b-color (atom (get-color :stroke-1))
         press-handler (fn [event]
                         (swap! status not)
                         (if handler
                           (handler @status))
                         (if @status
                           (sg/fill-color back (transparent-color @b-color))
                           (sg/fill-color back (get-color :background))))]
     (sg/observe b-color
       (fn [new-color]
         (if @status
          (sg/fill-color back (transparent-color new-color))
          (sg/fill-color back (get-color :background)))
         (sg/stroke-color box new-color)))

     (doto back
       (sg/mode :fill)
       (sg/fill-color (get-color :background))
       (sg/set-shape (sg/round-rectangle x-pos y-pos
                                            BUTTON-SIZE BUTTON-SIZE
                                            BUTTON-CORNER BUTTON-CORNER)))
     (doto box
       (sg/anti-alias :on)
       (sg/mode :stroke)
       (sg/stroke-color (get-color :stroke-1))
       (sg/set-shape (sg/round-rectangle x-pos y-pos
                                   BUTTON-SIZE BUTTON-SIZE
                                   BUTTON-CORNER BUTTON-CORNER)))
     (sg/add group back box)

     (sg/on-mouse group
       :press press-handler)

     {:type :button
      :group group
      :color b-color})))

