(ns
  ^{:doc "A simple button widget"
    :author "Jeff Rose"}
  overtone.gui.surface.button
  (:use
    [overtone event]
    [overtone.gui color]
    [overtone.gui.surface core])
  (:require [overtone.gui.sg :as sg]))

(def BUTTON-SIZE 20)
(def BUTTON-CORNER 5)

(defn- button*
  ([{:keys [value color]}]
   (let [group  (sg/group)
         box    (sg/shape)
         status (atom nil)
         b-color (atom (or color (get-color :stroke-1)))]

     (sg/observe b-color
       (fn [new-color]
         (if @status
          (sg/fill-color  box (transparent-color new-color))
          (sg/fill-color  box (get-color :background)))
         (sg/stroke-color box new-color)))

     (doto box
       (sg/anti-alias :on)
       (sg/mode :stroke-fill)
       (sg/fill-color (get-color :background))
       (sg/stroke-color @b-color)
       (sg/set-shape (sg/round-rectangle 0 0 
                                   BUTTON-SIZE BUTTON-SIZE
                                   BUTTON-CORNER BUTTON-CORNER)))
     (sg/add group box)

     (sg/observe status
       (fn [new-status]
         (if new-status
           (sg/fill-color box (transparent-color @b-color))
           (sg/fill-color box (get-color :background)))))

     (reset! status (if val true false))

     (sg/on-mouse group
       :press (fn [event] (swap! status not)))

     {:type :button
      :group group
      :color b-color
      :value status})))

(def button (widget-fn button*))
