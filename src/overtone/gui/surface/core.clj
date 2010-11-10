(ns
  ^{:doc "A control surface."
    :author "Jeff Rose"}
  overtone.gui.surface.core
  (:use
    [overtone event]
    [overtone.gui color])
  (:require [overtone.gui.sg :as sg])
  (:import [java.awt.geom AffineTransform]))

(defn grid-lines [width height]
  (let [grid-path (sg/path)]
    (doseq [x (range 0.0 width (/ width 10.0))] ; vertical
      (sg/move-to grid-path x 0.0)
      (sg/line-to grid-path x height))

    (doseq [y (range 0.0 height (/ height 10.0))] ; horizontal
      (sg/move-to grid-path 0.0 y)
      (sg/line-to grid-path width y))
    grid-path))

(defn- surface-group [surface]
  (let [{:keys [name width height]} surface
        group (sg/group)
        background (sg/shape)
        grid (sg/shape)
        grid-path (grid-lines width height)]

    (doto background
      (sg/mode :fill)
      (sg/fill-color (get-color :background))
      (sg/set-shape (sg/rectangle 0.0 0.0 width height)))

    (doto grid
      (sg/set-shape grid-path)
      (sg/anti-alias :on)
      (sg/mode :stroke)
      (sg/stroke-color (get-color :grid-lines)))

    (sg/add group
            background
            grid)

    (assoc surface
           :background background
           :grid grid
           :group group)))

(defn surface-mode [s mode]
  (reset! (:mode* s) mode))

(defn surface
  [name width height]
  (let [frame (sg/frame name width height)
        panel (sg/panel width height)
        surf {:type :surface
              :name name
              :width width
              :height height
              :widgets* (atom #{})}
        surf  (surface-group surf)
        zoom (sg/scale (:group surf) 1 1)
        mode* (atom :active)
        surf (assoc surf
                    :mode mode*
                    :frame frame
                    :panel panel
                    :zoom zoom)]
    (sg/set-scene panel zoom)

    (doto frame
      (.add panel)
      (.pack)
      (.show))

    (sg/on-key-pressed (:group surf)
      (fn [{:keys [key modifiers]}]
        (cond
          (and (= "Minus" key)       ; zoom out
               (= "Ctrl" modifiers))
          (.scaleBy zoom 0.9 0.9)

          (and (or (= "Equals" key)  ; zoom in
                   (= "Plus" key))
               (= "Ctrl" modifiers))
          (.scaleBy zoom 1.1 1.1)

          (= "E" key)
          (reset! mode* (if (= :edit @mode*)
                          :active
                          :edit)))))
    surf))

(defn select-widget [s widget]
  (doseq [w @(:widgets s)]
    (reset! (:selected? widget) false))
  (reset! (:selected? widget) true))

(defn surface-add-widget
  "Add a control widget to a surface, optionally specifying the
  x,y position and scaling factor for the widget."
  ([surface {:keys [width height] :as widget}]
   (surface-add-widget surface widget (/ width 2) (/ height 2)))
  ([surface widget x y]
   (surface-add-widget surface widget x y 1.0))
  ([surface widget x y scale-factor]
   (let [{:keys [group widgets]} surface
         affine (sg/affine (:group widget))
         bounds (.getBounds (:group widget))
         bounding-box (sg/shape)
         widget (assoc widget
                       :affine affine
                       :selected? (atom false)
                       :bounding-box bounding-box)]
     (.transformBy affine (AffineTransform/getTranslateInstance (double x) (double y)))

     (doto bounding-box
       (sg/set-shape (sg/rectangle (.getX bounds)
                                   (.getY bounds)
                                   (.getWidth bounds)
                                   (.getHeight bounds)))
       (sg/mode :stroke)
       (sg/stroke-color (get-color :bounding-box))
       (sg/stroke-style 2)
       (.setVisible false))

     (sg/add (:group widget) bounding-box)
     (add-watch (:selected? widget) (gensym "widget/selected?")
                (fn [_ _ _ selected?]
                  (.setVisible bounding-box selected?)))

     (let [last-x* (atom 0)
           last-y* (atom 0)
           press-handler
           (fn [event]
             (println "press..." @(:mode surface))
             (when (= :edit @(:mode surface))
               (println "edit select")
               (let [x (.getX event)
                     y (.getY event)]
                 (select-widget widget)

                 (reset! last-x* x)
                 (reset! last-y* y))))

           drag-handler
           (fn [event]
             (println "drag..." @(:mode surface))
             (when (= :edit @(:mode surface))
                 (println "edit drag")
                 (let [cur-x (.getX event)
                       dx (- cur-x @last-x*)
                       cur-y (.getY event)
                       dy (- cur-y @last-y*)]
                   (.transformBy affine
                                 (AffineTransform/getTranslateInstance (double dx) 
                                                                       (double dy)))
                   (reset! last-x* cur-x)
                   (reset! last-y* cur-y))))]

       (sg/on-mouse group
                    :press press-handler
                    :drag drag-handler))

     (sg/add group affine)

     (swap! (:widgets* surface) conj widget)
     widget)))

(defn surface-remove-widget [surface widget]
  (let [{:keys [group widgets]} surface
        {:keys [translate]} widget]
    (sg/remove group translate)
    (swap! widgets disj widget)
    surface))

