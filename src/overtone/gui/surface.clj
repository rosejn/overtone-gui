(ns
  ^{:doc "A control surface."
    :author "Jeff Rose"}
  overtone.gui.surface
  (:import
    (java.awt Dimension Color)
    (java.awt.event KeyEvent)
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

(def DEFAULT-WIDTH 640)
(def DEFAULT-HEIGHT 480)

(defn grid-lines [width height]
  (let [grid-path (Path2D$Float.)]
    (doseq [x (range 0.0 width (/ width 10.0))] ; vertical
      (.moveTo grid-path (float x) (float 0.0))
      (.lineTo grid-path (float x) (float height)))

    (doseq [y (range 0.0 height (/ height 10.0))] ; horizontal
      (.moveTo grid-path (float 0.0) (float y))
      (.lineTo grid-path (float width) (float y)))
    grid-path))

(defn- surface-group [surface]
  (let [{:keys [name width height]} surface
        group (sg-group)
        background (sg-shape)
        grid (sg-shape)
        grid-path (grid-lines width height)]

    (doto background
      (set-mode! :fill)
      (set-fill-paint! (get-color :background))
      (set-shape! (Rectangle2D$Float. 0.0 0.0 width height)))

    (doto grid
      (set-shape! grid-path)
      (set-antialias! :on)
      (set-mode! :stroke)
      (set-stroke-paint! (get-color :grid-lines)))

    (doto group
      (add! background)
      (add! grid))

    (assoc surface
           :background background
           :grid grid
           :group group)))

(defn edit-mode [s])

(defn active-mode [s])


(defn surface 
  ([name] (surface name DEFAULT-WIDTH DEFAULT-HEIGHT))
  ([name width height]
   (let [frame (JFrame. name)
         panel (sg-panel width height)
         surf  (surface-group {:type :surface
                                :name name
                                :width width
                                :height height
                                :widgets (atom #{})})
         zoom (scale (:group surf) 1 1)
         edit-mode-status (atom false)
         surf (assoc surf
                     :frame frame
                     :panel panel
                     :zoom zoom)
         ctl-status (atom false)]
     (set-scene! panel zoom)

     (doto frame
       (.setPreferredSize (Dimension. width height))
       (.add panel)
       (.pack)
       (.show))

     (on-key-pressed (:group surf)
       (fn [event]
         (let [key (.getKeyCode event)]
               (cond 
                 (= KeyEvent/VK_CONTROL key)          ; ctl-down
                 (reset! ctl-status true)

                 (and (= KeyEvent/VK_MINUS key)       ; zoom out
                      @ctl-status)
                 (.scaleBy zoom 0.9 0.9)

                 (and (or (= KeyEvent/VK_EQUALS key)  ; zoom in
                          (= KeyEvent/VK_PLUS key))
                      @ctl-status)
                 (.scaleBy zoom 1.1 1.1)

                 (and (= KeyEvent/VK_E key)
                      @ctl-status)
                 (swap! edit-mode-status not)))))

     (add-watch edit-mode-status (gensym "surface-edit-mode")
       (fn [_ _ _ edit?]
         (if edit?
           (edit-mode surf)
           (active-mode surf))))

     surf)))

(defn surface-add-widget 
  "Add a control widget to a surface, optionally specifying the
  x,y position and scaling factor for the widget."
  ([surface {:keys [width height] :as widget}] 
   (surface-add-widget surface widget (/ width 2) (/ height 2)))
  ([surface widget x y]
   (surface-add-widget surface widget x y 1.0))
  ([surface widget x y scale-factor]
   (let [{:keys [group widgets]} surface
         w-scale (scale (:group widget) scale-factor scale-factor)
         w-translate (translate w-scale x y)
         widget (assoc widget 
                       :scale w-scale
                       :translate w-translate)]
     (add! group w-translate)
     (swap! widgets conj widget)
     surface)))

(defn surface-remove-widget [surface widget]
  (let [{:keys [group widgets]} surface
        {:keys [translate]} widget]
    (remove! group translate)
    (swap! widgets disj widget)
    surface))

