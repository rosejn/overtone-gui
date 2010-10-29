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

(def FADER-WIDTH 20)
(def FADER-HEIGHT 140)
(def FADER-CORNER-FADER-WIDTH 5)
(def FADER-CORNER-FADER-HEIGHT 5)

(defn fader
  ([] (fader false))
  ([handler]
   (let [group (sg-group)
         box (sg-shape)
         handle-height (/ FADER-HEIGHT 15)
         handle (sg-shape)
         handle-tx (translate handle 0 0)
         slide (sg-shape)
         slide-scale (scale slide 1 (/ (- FADER-HEIGHT handle-height) FADER-HEIGHT))
         slide-tx (translate slide-scale 1 handle-height)
         value (atom 0.8)
         last-y (atom 0)]
     (doto box
       (set-antialias! :on)
       (set-mode! :stroke)
       (set-stroke-paint! (get-color :stroke-1))
       (set-shape! (RoundRectangle2D$Float. 0 0 FADER-WIDTH FADER-HEIGHT
                                            FADER-CORNER-FADER-WIDTH
                                            FADER-CORNER-FADER-HEIGHT)))

     (doto slide
       (set-antialias! :on)
       (set-mode! :fill)
       (set-fill-paint! (get-color :fill-1))
       (set-shape! (RoundRectangle2D$Float. 0 0
                                            FADER-WIDTH
                                            FADER-HEIGHT
                                            FADER-CORNER-FADER-WIDTH
                                            FADER-CORNER-FADER-HEIGHT)))
     (doto handle
       (set-antialias! :on)
       (set-mode! :fill)
       (set-fill-paint! (get-color :stroke-1))
       (set-shape! (RoundRectangle2D$Float. 0 0
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
       (on-mouse-pressed handle press-handler)
       (on-mouse-pressed slide press-handler)
       (on-mouse-dragged handle drag-handler)
       (on-mouse-dragged slide drag-handler))

     (doto group
       (add! slide-tx)
       (add! handle-tx)
       (add! box))

     {:type :fader
      :group group
      :value value})))

(defn fader-panel []
  (let [p (sg-panel 400 400)
        group (sg-group)
        background (sg-shape)]
    (doto background
      (set-mode! :fill)
      (set-fill-paint! (get-color :background))
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
