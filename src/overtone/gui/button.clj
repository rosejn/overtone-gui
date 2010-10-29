(ns
  ^{:doc "A simple button widget"
    :author "Jeff Rose"}
  overtone.gui.button
  (:import
    (java.awt Dimension Color)
    (com.sun.scenario.scenegraph JSGPanel SGText SGShape SGGroup)
    (com.sun.scenario.scenegraph.fx FXShape FXText)
    (java.awt.geom Rectangle2D$Float RoundRectangle2D$Float)
    (javax.swing JFrame))
  (:use
    clojure.stacktrace
    [overtone event]
    [overtone.gui sg color surface]))

(def BUTTON-SIZE 20)
(def BUTTON-CORNER 5)

(defn button
  ([] (button false))
  ([handler]
   (let [group  (sg-group)
         back   (sg-shape)
         box    (sg-shape)
         x-pos  0
         y-pos  0
         status (atom false)
         b-color (atom (get-color :stroke-1))
         press-handler (fn [event]
                         (swap! status not)
                         (if handler
                           (handler @status))
                         (if @status
                           (set-fill-paint! back (fill-color @b-color))
                           (set-fill-paint! back (get-color :background))))]
     (add-watch b-color (gensym "button-color")
       (fn [_ _ _ new-color]
         (if @status
           (set-fill-paint! back (fill-color new-color))
           (set-fill-paint! back (get-color :background)))
         (set-stroke-paint! box new-color)))

     (doto back
       (set-mode! :fill)
       (set-fill-paint! (get-color :background))
       (set-shape! (RoundRectangle2D$Float. x-pos y-pos
                                            BUTTON-SIZE BUTTON-SIZE
                                            BUTTON-CORNER BUTTON-CORNER)))
     (doto box
       (set-antialias! :on)
       (set-mode! :stroke)
       (set-stroke-paint! (get-color :stroke-1))
       (set-shape! (RoundRectangle2D$Float. x-pos y-pos
                                            BUTTON-SIZE BUTTON-SIZE
                                            BUTTON-CORNER BUTTON-CORNER)))
     (doto group
       (.add back)
       (.add box))
     (on-mouse-pressed back press-handler)
     (on-mouse-pressed box press-handler)

     {:type :button
      :group group
      :color b-color})))

