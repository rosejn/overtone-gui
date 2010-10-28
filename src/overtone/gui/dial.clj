(ns
  ^{:doc "A radial dial control"
    :author "Jeff Rose"}
  overtone.gui.dial
  (:import
    (java.awt Dimension Color)
    (com.sun.scenario.scenegraph JSGPanel SGText SGShape SGGroup
                                 SGTransform SGComponent SGAbstractShape$Mode)
    (com.sun.scenario.scenegraph.fx FXShape FXText)
    (com.sun.scenario.scenegraph.event SGMouseAdapter)
    (java.awt.geom Rectangle2D$Float Arc2D$Float Arc2D)
    (javax.swing JFrame))
  (:use
    clojure.stacktrace
    [overtone event]
    [overtone.gui swing sg color]))

(def SIZE 50)

(defn dial
  ([] (dial false))
  ([handler]
   (let [group (sg-group)
         ring (sg-shape)
         back-fill (sg-shape)
         front-fill (sg-shape)
         fill-arc (Arc2D$Float. 0 0 SIZE SIZE
                                 -130 -200 Arc2D/PIE)
         center (sg-shape)
         value (atom 0.8)
         start-y (atom 0)]
     (doto ring
       (set-antialias! :on)       
       (set-mode! :stroke)
       (set-draw-stroke! 1.5)
       (set-stroke-paint! (color :stroke-1))
       (set-shape! (Arc2D$Float. 0 0 SIZE SIZE -130 -280 Arc2D/OPEN)))
     (doto back-fill
       (set-antialias! :on)       
       (set-mode! :fill)
       (set-fill-paint! (color :fill-1))
       (set-shape! (Arc2D$Float. 0 0 SIZE SIZE
                                 -130 -280 Arc2D/PIE)))
     (doto front-fill
       (set-antialias! :on)       
       (set-mode! :fill)
       (set-fill-paint! (color :stroke-1))
       (set-shape! fill-arc))
     (doto center
       (set-antialias! :on)       
       (set-mode! :fill)
       (set-fill-paint! (color :background))
       (set-shape! (Arc2D$Float. (/ SIZE 4)
                                 (/ SIZE 4)
                                 (/ SIZE 2)
                                 (/ SIZE 2)
                                 0 360 Arc2D/OPEN)))
     (add-watch value :gensym 
        (fn [_ _ _ new-val]
          (let [angle (- (* new-val 280))]
            (doto fill-arc
              (.setAngleExtent angle))
            (set-shape! front-fill fill-arc))))

     (reset! value 0.5)

     (let [press-handler
           (fn [event]
             (let [y (.getY event)]
               (reset! start-y y)))

           drag-handler
           (fn [event]
             (let [cur-y (.getY event)
                   dy (- cur-y @start-y)
                   val (max 0 (min (* 0.01 dy) 1))]
               (reset! value val)
               (if handler
                 (handler val))))]
       (on-mouse-pressed back-fill press-handler)
       (on-mouse-pressed front-fill press-handler)
       (on-mouse-dragged center press-handler)
       (on-mouse-dragged back-fill drag-handler)
       (on-mouse-dragged front-fill drag-handler)
       (on-mouse-dragged center drag-handler))

     (doto group
       (.add back-fill)
       (.add front-fill)
       (.add center)
       (.add ring))

     {:type :dial
      :group group
      :value value})))

(defn dial-panel []
  (let [p (sg-panel 400 400)
        group (sg-group)
        background (sg-shape)]
    (doto background
      (set-mode! :fill)
      (set-fill-paint! (color :background))
      (set-shape! (Rectangle2D$Float. 0.0 0.0 400 400)))
    (.add group background)

    (dotimes [i 4]
      (.add group (translate (:group (dial #(println %)) (+ 50 (* i 60)) 50)))

    (set-scene! p group)
    p))

(defn dial-frame []
  (let [f (JFrame. "Dial Test")]
    (doto f
      (.setPreferredSize (Dimension. 400 400))
      (.add (dial-panel))
      (.pack)
      (.show))))
