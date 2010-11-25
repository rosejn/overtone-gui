(ns
  ^{:doc "A radial dial control"
    :author "Jeff Rose"}
  overtone.gui.surface.dial
  (:use
    [overtone event]
    [overtone.gui color]
    [overtone.gui.surface core])
  (:require [overtone.gui.sg :as sg]))

(def DIAL-SIZE 30)

(defn- dial*
  ([{:keys [value color]}]
   (let [group (sg/group)
         ring (sg/shape)
         back-fill (sg/shape)
         front-fill (sg/shape)
         fill-arc (sg/arc 0 0 DIAL-SIZE DIAL-SIZE
                                 -130 -200 :pie)
         center (sg/shape)
         value (atom value)
         d-color (atom (or color (get-color :stroke-1)))]
     (doto ring
       (sg/anti-alias :on)
       (sg/mode :stroke)
       (sg/stroke-style 1.5)
       (sg/stroke-color @d-color)
       (sg/set-shape (sg/arc 0 0 DIAL-SIZE DIAL-SIZE -130 -280 :pie)))
     (doto back-fill
       (sg/anti-alias :on)
       (sg/mode :fill)
       (sg/fill-color (transparent-color @d-color))
       (sg/set-shape (sg/arc 0 0 DIAL-SIZE DIAL-SIZE
                                 -130 -280 :pie)))
     (doto front-fill
       (sg/anti-alias :on)
       (sg/mode :fill)
       (sg/fill-color @d-color)
       (sg/set-shape fill-arc))
     (doto center
       (sg/anti-alias :on)
       (sg/mode :fill)
       (sg/fill-color (get-color :background))
       (sg/set-shape (sg/arc (/ DIAL-SIZE 4)
                             (/ DIAL-SIZE 4)
                             (/ DIAL-SIZE 2)
                             (/ DIAL-SIZE 2)
                             0 360 :open)))
     (sg/observe value
        (fn [new-val]
          (let [angle (- (* new-val 280))]
            (.setAngleExtent fill-arc angle)
            (sg/set-shape front-fill fill-arc))))

     (sg/add group
             ring
             back-fill
             front-fill
             center)

     (sg/observe d-color
       (fn [new-color]
         (sg/stroke-color ring new-color)
         (sg/fill-color back-fill (transparent-color new-color))
         (sg/fill-color front-fill new-color)))

     (let [last-y (atom 0)
           press-handler
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
               (reset! value val)
               (reset! last-y cur-y)))]

       (sg/on-mouse group
         :press press-handler
         :drag drag-handler))

     {:type :dial
      :group group
      :value value
      :color d-color})))

(def dial (widget-fn dial*))
