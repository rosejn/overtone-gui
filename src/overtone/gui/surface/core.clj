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

(defn- with-surface-group [surface]
  (let [{:keys [name width height]} surface
        group (sg/group)
        background (sg/shape)
        grid (sg/shape)
        grid-path (grid-lines width height)]

    ; TODO: Figure out how to have a seemingly infinite background
    (doto background
      (sg/mode :fill)
      (sg/fill-color (get-color :background))
      (sg/set-shape (sg/rectangle 0 0 width height)))
      ;(* -2.0 width) (* -2.0 height)
      ;(* 2.0 width) (* 2.0 height))))

    (doto grid
      (sg/set-shape grid-path)
      (sg/anti-alias :on)
      (sg/mode :stroke)
      (sg/stroke-color (get-color :grid-lines))
      (.setVisible false))

    (sg/add group
            background
            grid)

    (assoc surface
           :background background
           :grid grid
           :group group)))

(defn surface-mode [s mode]
  (reset! (:mode* s) mode))

(defn select-widget [s widget]
  (reset! (:selected?* widget) true))

(defn deselect-widget [s widget]
  (reset! (:selected?* widget) false))

(defn deselect-all-widgets [s]
  (doseq [w @(:widgets* s)]
    (deselect-widget s w)))

(defn selected-widgets [s]
  (filter (fn [widget] @(:selected?* widget))
          @(:widgets* s)))

(defn select-intersecting-widgets [s rect]
  (doseq [w (filter #(.intersects rect (.getBounds (:affine %))) 
                    @(:widgets* s))]
    (select-widget s w)))

(defn surface
  [name width height]
  (let [frame (sg/frame name width height)
        panel (sg/panel width height)
        surf {:type :surface
              :name name
              :width width
              :height height
              :widgets* (atom #{})}
        surf  (with-surface-group surf)
        affine (sg/affine (:group surf))
        mode* (atom :active)
        selection-box (sg/shape)
        selection-rect (sg/rectangle 0 0 0 0)
        surf (assoc surf
                    :mode* mode*
                    :frame frame
                    :panel panel
                    :selection-box selection-box
                    :affine affine)]
    (sg/set-scene panel affine)

    (doto frame
      (.add panel)
      (.pack)
      (.show))

    (doto selection-box
      (sg/anti-alias :on)
      (sg/mode :stroke-fill)
      (sg/stroke-color (color 255 255 255 150))
      (sg/fill-color (color 255 255 255 50))
      (sg/set-shape selection-rect)
      (.setVisible false))

    (let [select-x* (atom 0)
          select-y* (atom 0)
          press-handler
           (fn [event]
             (when (= :edit @mode*)
               (let [x (.getX event)
                     y (.getY event)]
                 (sg/add (:group surf) selection-box)
                 (.setRect selection-rect x y 1 1)
                 (sg/set-shape selection-box selection-rect)
                 (.setVisible selection-box true)
                 (reset! select-x* x)
                 (reset! select-y* y))))

          click-handler
           (fn [event]
             (when (= :edit @mode*)
               (if (not (.isShiftDown event))
                 (deselect-all-widgets surf))))

           drag-handler
           (fn [event]
             (when (= :edit @mode*)
               (let [x (.getX event)
                     y (.getY event)
                     rect-x (min x @select-x*)
                     rect-y (min y @select-y*)
                     width (- (max x @select-x*) rect-x)
                     height (- (max y @select-y*) rect-y)]
                 (println "rect: [" rect-x "," rect-y "] - w: " width " h: " height)
                 (.setRect selection-rect rect-x rect-y width height)
                 (sg/set-shape selection-box selection-rect))))

          release-handler
           (fn [event]
             (when (= :edit @mode*)
               (.setVisible selection-box false)
               (sg/remove (:group surf) selection-box)
               (try 
                 (if (not (.isShiftDown event))
                   (deselect-all-widgets surf))
                 (select-intersecting-widgets surf selection-rect)
                 (catch Exception e
                   (println "exception: " e)
                   (println (.printStackTrace e))))))]

       (sg/on-mouse (:group surf)
                    :click click-handler
                    :drag  drag-handler
                    :release release-handler
                    :press press-handler))

    (sg/on-key-pressed (:group surf)
      (fn [{:keys [key modifiers]}]
        (cond
          (and (= "Minus" key)       ; zoom out
               (= "Ctrl" modifiers))
          (.transformBy affine (AffineTransform/getScaleInstance 0.9 0.9))

          (and (or (= "Equals" key)  ; zoom in
                   (= "Plus" key))
               (= "Ctrl" modifiers))
          (.transformBy affine (AffineTransform/getScaleInstance 1.1 1.1))

          (= "E" key)
          (if (= :edit @mode*)
            (surface-mode surf :active)
            (surface-mode surf :edit)))))
    surf))

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
                       :selected?* (atom false)
                       :bounding-box bounding-box)]
     (.transformBy affine (AffineTransform/getTranslateInstance (double x) (double y)))

     (doto bounding-box
       (sg/set-shape (sg/rectangle (.getX bounds)
                                   (.getY bounds)
                                   (.getWidth bounds)
                                   (.getHeight bounds)))
       (sg/mode :stroke)
       (sg/stroke-color (get-color :bounding-box))
       (sg/stroke-style 0.5)
       (.setVisible false))

     (sg/add (:group widget) bounding-box)
     (add-watch (:selected?* widget) (gensym "widget/selected?*")
                (fn [_ _ _ selected?]
                  (.setVisible bounding-box selected?)))

     (add-watch (:mode* surface) (gensym "widget/edit-mode")
       (fn [_ _ _ mode]
         nil
         (comment if (= :edit mode)
           (.setMouseBlocker affine true)
           (.setMouseBlocker affine false))))

     (let [last-x* (atom 0)
           last-y* (atom 0)
           press-handler
           (fn [event]
             (when (= :edit @(:mode* surface))
               (let [x (.getX event)
                     y (.getY event)]
                 (try
                   (if (.isShiftDown event)
                     (select-widget surface widget)
                     (do
                       (deselect-all-widgets surface)
                       (select-widget surface widget)))
                   (catch Exception e
                     (println "select-widget exception: " e)
                     (println (.printStackTrace e))))

                 (reset! last-x* x)
                 (reset! last-y* y))))

           drag-handler
           (fn [event]
             (when (= :edit @(:mode* surface))
                 (let [cur-x (.getX event)
                       dx (- cur-x @last-x*)
                       cur-y (.getY event)
                       dy (- cur-y @last-y*)]
                   (try
                     (doseq [w (selected-widgets surface)]
                       (.transformBy (:affine w)
                                     (AffineTransform/getTranslateInstance (double dx)
                                                                           (double dy))))
                   (catch Exception e
                     (println "move-widgets exception: " e)
                     (println (.printStackTrace e))))

                   (reset! last-x* cur-x)
                   (reset! last-y* cur-y))))]

       (sg/on-mouse affine
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

