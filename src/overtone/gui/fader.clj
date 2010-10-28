(ns
  ^{:doc "A fader control"
    :author "Jeff Rose"}
  overtone.gui.fader
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

(def WIDTH 20)
(def HEIGHT 120)
(def CORNER-WIDTH 5)
(def CORNER-HEIGHT 5)

(defn fader
  ([] (fader false))
  ([handler]
   (let [group (sg-group)
         box (sg-shape)
         handle-height (/ HEIGHT 15)
         handle (sg-shape)
         handle-tx (translate handle 0 0)
         slide (sg-shape)
         slide-scale (scale slide 1 (/ (- HEIGHT handle-height) HEIGHT))
         slide-tx (translate slide-scale 1 handle-height)
         value (atom 0.8)
         last-y (atom 0)]
     (doto box
       (set-antialias! :on)
       (set-mode! :stroke)
       (set-stroke-paint! (color :stroke-1))
       (set-shape! (RoundRectangle2D$Float. 0 0 WIDTH HEIGHT
                                            CORNER-WIDTH
                                            CORNER-HEIGHT)))

     (doto slide
       (set-antialias! :on)
       (set-mode! :fill)
       (set-fill-paint! (color :fill-1))
       (set-shape! (RoundRectangle2D$Float. 1 0
                                            (- WIDTH 2)
                                            HEIGHT
                                            CORNER-WIDTH
                                            CORNER-HEIGHT)))
     (doto handle
       (set-antialias! :on)
       (set-mode! :fill)
       (set-fill-paint! (color :stroke-1))
       (set-shape! (RoundRectangle2D$Float. 1 0
                                            (- WIDTH 2)
                                            handle-height
                                            (/ CORNER-WIDTH 3)
                                            (/ CORNER-HEIGHT 3))))
     (let [press-handler
           (fn [event]
             (let [y (.getY event)]
               (reset! last-y y)))

           drag-handler
           (fn [event]
             (let [cur-y (.getY event)
                   dy (- cur-y @last-y)
                   cur-ty (.getTranslateY handle-tx)
                   y (+ cur-ty dy)
                   y (max 0 (min (- HEIGHT handle-height) y))
                   scale (float (/ (- HEIGHT y) HEIGHT))
                   val (float (/ (- HEIGHT handle-height y)
                                 (- HEIGHT handle-height)))]
               (.setTranslateY handle-tx y)
               (.setTranslateY slide-tx y)
               (.setScaleY slide-scale scale)
               (reset! last-y cur-y)
               (reset! value val)
               (if handler
                 (handler val))))]
       (on-mouse-pressed handle press-handler)
       (on-mouse-pressed slide press-handler)
       (on-mouse-dragged handle drag-handler)
       (on-mouse-dragged slide drag-handler))

     (doto group
       (.add slide-tx)
       (.add box)
       (.add handle-tx))

     {:type :fader
      :group group
      :value value})))

(defn fader-panel []
  (let [p (sg-panel 400 400)
        group (sg-group)
        background (sg-shape)]
    (doto background
      (set-mode! :fill)
      (set-fill-paint! (color :background))
      (set-shape! (Rectangle2D$Float. 0.0 0.0 400 400)))
    (.add group background)
    (dotimes [i 10]
      (.add group (translate (:group (fader)) (+ 50 (* i 30)) 50)))
    (set-scene! p group)
    p))

(defn fader-frame []
  (let [f (JFrame. "Fader Test")]
    (doto f
      (.setPreferredSize (Dimension. 400 400))
      (.add (fader-panel))
      (.pack)
      (.show))))
