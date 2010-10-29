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

(def MONOME-BUTTON-SIZE 25)
(def MONOME-MARGIN 6)
(def FMONOME-BUTTON-SIZE (+ MONOME-BUTTON-SIZE MONOME-MARGIN))
(def MONOME-CORNER 3)

(defn m-button [monome x y]
  (let [{:keys [color handler]} monome
        group  (sg-group)
        back   (sg-shape)
        box    (sg-shape)
        x-pos  (+ (* x FMONOME-BUTTON-SIZE) MONOME-MARGIN)
        y-pos  (+ (* y FMONOME-BUTTON-SIZE) MONOME-MARGIN)
        status (atom false)
        press-handler (fn [event]
                        (swap! status not)
                        (if handler
                          (handler x y @status))
                        (if @status
                          (set-fill-paint! back (fill-color @color))
                          (set-fill-paint! back (get-color :background))))]
    (doto back
      (set-mode! :fill)
      (set-fill-paint! (get-color :background))
      (set-shape! (RoundRectangle2D$Float. x-pos y-pos
                                           MONOME-BUTTON-SIZE MONOME-BUTTON-SIZE
                                           MONOME-CORNER MONOME-CORNER)))

    (add-watch color (gensym "monome-color")
               (fn [_ _ _ new-color]
                 (if @status
                   (set-fill-paint! back (fill-color new-color))
                   (set-fill-paint! back (get-color :background)))
                 (set-stroke-paint! box new-color)))

    (doto box
      (set-antialias! :on)
      (set-mode! :stroke)
      (set-stroke-paint! @color)
      (set-shape! (RoundRectangle2D$Float. x-pos y-pos
                                           MONOME-BUTTON-SIZE MONOME-BUTTON-SIZE
                                           MONOME-CORNER MONOME-CORNER)))
    (doto group
      (.add back)
      (.add box))
    (on-mouse-pressed back press-handler)
    (on-mouse-pressed box press-handler)
    group))

(defn monome
  ([columns rows] (monome columns rows false))
  ([columns rows handler]
   (let [width   (+ MONOME-MARGIN (* columns FMONOME-BUTTON-SIZE))
         height  (+ MONOME-MARGIN (* rows FMONOME-BUTTON-SIZE))
         group   (sg-group)
         border  (sg-shape)
         m-color (atom (get-color :stroke-1))
         mono    {:type :monome
                  :group group
                  :columns columns
                  :rows rows
                  :handler handler
                  :color m-color}
         buttons (doall (for [i (range columns)
                              j (range rows)]
                          (m-button mono i j)))]
     (doto border
       (set-mode! :stroke)
       (set-stroke-paint! @m-color)
       (set-shape! (RoundRectangle2D$Float. 0 0 width height
                                            MONOME-CORNER 
                                            MONOME-CORNER)))
     (add! group border)

     (doseq [button buttons]
       (add! group button))

     mono)))

(defn monome-panel []
  (let [x 10
        y 10
        scale-factor 1.5
        width (* scale-factor x FMONOME-BUTTON-SIZE)
        height (* scale-factor y FMONOME-BUTTON-SIZE)
        p (sg-panel width height)
        group (sg-group)
        background (sg-shape)
        handler (fn [x y state] (println x y state))]
    (doto background
      (set-mode! :fill)
      (set-fill-paint! (get-color :background))
      (set-shape! (Rectangle2D$Float. 0.0 0.0 width height)))
    (doto group
      (.add background)
      (.add (scale (:group (monome 10 10 handler)) 
                   scale-factor scale-factor)))
    (set-scene! p group)
    p))

(defn monome-frame []
  (let [f (JFrame. "Monome Test")]
    (doto f
      (.setPreferredSize (Dimension. 400 400))
      (.add (monome-panel))
      (.pack)
      (.show))))
