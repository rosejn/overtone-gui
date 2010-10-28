(ns
  ^{:doc "A control surface."
    :author "Jeff Rose"}
  overtone.gui.surface
  (:import
    (java.awt Dimension Color)
    (com.sun.scenario.scenegraph JSGPanel SGText SGShape SGGroup
                                 SGTransform SGComponent SGAbstractShape$Mode)
    (com.sun.scenario.scenegraph.fx FXShape FXText)
    (com.sun.scenario.scenegraph.event SGMouseAdapter)
    (java.awt.geom Path2D$Float Rectangle2D$Float Arc2D$Float Arc2D)
    (javax.swing JFrame))
  (:use
    clojure.stacktrace
    [overtone event]
    [overtone.gui swing sg color]))

(defn surface []
  (let []))

(defn grid-lines [width height]
  (let [grid-path (Path2D$Float.)]
    (doseq [x (range 0.0 width (/ width 10.0))] ; vertical
      (.moveTo grid-path x 0.0)
      (.lineTo grid-path x height))

    (doseq [y (range 0.0 height (/ height 10.0))] ; horizontal
      (.moveTo grid-path 0.0 y)
      (.lineTo grid-path width y))
    grid-path))

(defn surface-panel [width height]
  (let [p (sg-panel width height)
        group (sg-group)
        background (sg-shape)
        grid (sg-shape)
        grid-path (grid-lines width height)]

    (doto background
      (set-mode! :fill)
      (set-fill-paint! (color :background))
      (set-shape! (Rectangle2D$Float. 0.0 0.0 width height)))

    (doto grid
      (set-shape! grid-path)
      (set-antialias! :on)
      (set-mode! :stroke)
      (set-stroke-paint! (color :grid-lines)))

    (doto group
      (.add background)
      (.add grid))

    (set-scene! p group)
    p))

(defn surface-frame []
  (let [f (JFrame. "Surface")]
    (doto f
      (.setPreferredSize (Dimension. 400 400))
      (.add (surface-panel 400.0 400.0))
      (.pack)
      (.show))))
