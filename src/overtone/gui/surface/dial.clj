(ns
  ^{:doc "A radial dial control"
    :author "Jeff Rose"}
  overtone.gui.surface.dial
  (:use
    [overtone event]
    [overtone.gui color])
  (:require [overtone.gui.sg :as sg]))

(def DIAL-SIZE 30)

(defn dial
  ([] (dial false))
  ([handler]
   (let [group (sg/group)
         ring (sg/shape)
         back-fill (sg/shape)
         front-fill (sg/shape)
         fill-arc (sg/arc 0 0 DIAL-SIZE DIAL-SIZE
                                 -130 -200 :pie)
         center (sg/shape)
         value (atom 0.8)
         last-y (atom 0)
         d-color (atom (get-color :stroke-1))]
     (doto ring
       (sg/anti-alias :on)
       (sg/mode :stroke)
       (sg/stroke-style 1.5)
       (sg/stroke-color @d-color)
       (sg/set-shape (sg/arc 0 0 DIAL-SIZE DIAL-SIZE -130 -280 :open)))
     (doto back-fill
       (sg/anti-alias :on)
       (sg/mode :fill)
       (sg/fill-color (fill-color @d-color))
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
     (add-watch value (gensym "dial")
        (fn [_ _ _ new-val]
          (let [angle (- (* new-val 280))]
            (.setAngleExtent fill-arc angle)
            (sg/set-shape front-fill fill-arc))))

     (add-watch d-color (gensym "dial-color")
       (fn [_ _ _ new-color]
         (sg/stroke-color ring new-color)
         (sg/fill-color back-fill (fill-color new-color))
         (sg/fill-color front-fill new-color)))

     (reset! value 0.5)

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
               (reset! value val)
               (reset! last-y cur-y)
               (if handler
                 (handler val))))]

       (sg/on-mouse-pressed back-fill press-handler)
       (sg/on-mouse-pressed front-fill press-handler)
       (sg/on-mouse-dragged center press-handler)
       (sg/on-mouse-dragged back-fill drag-handler)
       (sg/on-mouse-dragged front-fill drag-handler)
       (sg/on-mouse-dragged center drag-handler))

     (sg/add group
       back-fill
       front-fill
       center
       ring)

     {:type :dial
      :group group
      :value value
      :color d-color})))
