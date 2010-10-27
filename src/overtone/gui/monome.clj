(ns
  ^{:doc "A monome control"
    :author "Jeff Rose"} 
  overtone.gui.monome
  (:import
    (java.awt Dimension Color)
    (com.sun.scenario.scenegraph JSGPanel SGText SGShape SGGroup 
                                 SGTransform SGComponent SGAbstractShape$Mode)
    (com.sun.scenario.scenegraph.fx FXShape FXText)
    (com.sun.scenario.scenegraph.event SGMouseAdapter)
    (java.awt.geom Rectangle2D$Float RoundRectangle2D$Float)
    (javax.swing JFrame))
  (:use
    clojure.stacktrace
    [overtone event]
    [overtone.gui swing sg color]))

(def SIZE 20)
(def MARGIN 2)
(def FSIZE (+ SIZE MARGIN))
(def CORNER 3)

(defn m-button [x y handler]
  (let [group (sg-group)
        back (sg-shape)
        box (sg-shape)
        x-pos (* x FSIZE)
        y-pos (* y FSIZE)
        status (atom false)
        press-handler (fn [event]
                        (if handler
                          (handler x y (swap! status not)))
                        (if @status
                          (set-fill-paint! back (color :fill-1))
                          (set-fill-paint! back (color :background)))
                        (println "toggle: " @status))]
    (doto back
      (set-mode! :fill)
      (set-fill-paint! (color :background))
      (set-shape! (RoundRectangle2D$Float. x-pos y-pos 
                                           SIZE SIZE 
                                           CORNER CORNER)))
    (doto box
      (set-mode! :stroke)
      (set-stroke-paint! (color :stroke-1))
      (set-shape! (RoundRectangle2D$Float. x-pos y-pos
                                           SIZE SIZE 
                                           CORNER CORNER)))
    (doto group
      (.add back)
      (.add box))
    (on-mouse-pressed back press-handler)
    (on-mouse-pressed box press-handler)
    group))

(defn monome 
  ([x y] (monome x y false))
  ([x y handler]
   (let [width (* x FSIZE)
         height (* y FSIZE)
         group (sg-group)
         border (sg-shape)
         buttons (doall (for [i (range x)
                              j (range y)]
                          (m-button i j handler)))]
     (doto border
       (set-mode! :stroke)
       (set-draw-paint! (color :stroke-1))
       (set-shape! (RoundRectangle2D$Float. 0 0 width height 
                                            CORNER CORNER)))
     (.add group border)

     (doseq [button buttons]
       (.add group button))

     group)))


(defn monome-panel []
  (let [x 10
        y 10
        scale-factor 1.5
        width (* scale-factor x FSIZE)
        height (* scale-factor y FSIZE)
        p (sg-panel width height)
        group (sg-group)
        background (sg-shape)
        handler (fn [x y state] (println x y state))]
    (doto background
      (set-mode! :fill)
      (set-fill-paint! (color :background))
      (set-shape! (Rectangle2D$Float. 0.0 0.0 width height)))
    (doto group
      (.add background)
      (.add (scale (monome 10 10 handler) scale-factor scale-factor)))
    (set-scene! p group)
    p))

(defn monome-frame []
  (let [f (JFrame. "Monome Test")]
    (doto f
      (.setPreferredSize (Dimension. 400 400))
      (.add (monome-panel))
      (.pack)
      (.show))))
