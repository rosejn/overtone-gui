(ns
  ^{:doc "A control surface."
    :author "Jeff Rose"}
  overtone.gui.surface.core
  (:use
    [overtone event]
    [overtone.gui color])
  (:require [overtone.gui.sg :as sg]))

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

(defn edit-mode [s])

(defn active-mode [s])

(defn surface
  [name width height]
  (let [frame (sg/frame name width height)
        panel (sg/panel width height)
        surf  (surface-group {:type :surface
                              :name name
                              :width width
                              :height height
                              :widgets (atom #{})})
        zoom (sg/scale (:group surf) 1 1)
        edit-mode-status (atom false)
        surf (assoc surf
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
        (println "key: " key modifiers)
        (cond
          (and (= "Minus" key)       ; zoom out
               (= "Ctrl" modifiers))
          (.scaleBy zoom 0.9 0.9)

          (and (or (= "Equals" key)  ; zoom in
                   (= "Plus" key))
               (= "Ctrl" modifiers))
          (.scaleBy zoom 1.1 1.1)

          (and (= "e" key)
               (= "Ctrl" modifiers))
          (swap! edit-mode-status not))))

    (add-watch edit-mode-status (gensym "surface-edit-mode")
               (fn [_ _ _ edit?]
                 (if edit?
                   (edit-mode surf)
                   (active-mode surf))))

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
         w-scale (sg/scale (:group widget) scale-factor scale-factor)
         w-translate (sg/translate w-scale x y)
         widget (assoc widget
                       :scale w-scale
                       :translate w-translate)]
     (sg/add group w-translate)
     (swap! widgets conj widget)
     surface)))

(defn surface-remove-widget [surface widget]
  (let [{:keys [group widgets]} surface
        {:keys [translate]} widget]
    (sg/remove group translate)
    (swap! widgets disj widget)
    surface))

