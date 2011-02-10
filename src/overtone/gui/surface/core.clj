(ns
  ^{:doc "A control surface."
    :author "Jeff Rose"}
  overtone.gui.surface.core
  (:use
    [overtone event]
    [overtone.gui color]
    clojure.contrib.repl-utils
    [overtone.gui.surface label])
  (:require [overtone.gui.sg :as sg])
  (:import [java.awt.geom AffineTransform]))

(def GROUP-PAD 5)
(def EDIT-PADDING 0)
(def SNAP-DISTANCE 10)

(defonce widget-fns* (atom {}))
(defonce widget-keys* (atom {}))

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

    (doto background
      (sg/mode :fill)
      (sg/fill-color (get-color :background))
      (sg/set-shape (sg/rectangle 0 0 width height)))

    (doto grid
      (sg/set-shape grid-path)
      (sg/anti-alias :on)
      (sg/mode :stroke)
      (sg/stroke-color (get-color :grid-lines))
      (sg/visible false))

    (sg/add group
            background
            grid)

    (assoc surface
           :background background
           :grid grid
           :group group)))

(defn select-widget [s widget]
  (reset! (:selected?* widget) true)
  (event ::widget-selected :surface s :widget widget))

(defn deselect-widget [s widget]
  (reset! (:selected?* widget) false))

(defn deselect-all-widgets [s]
  (doseq [w @(:widgets* s)]
    (deselect-widget s w)))

(defn selected-widgets [s]
  (filter (fn [widget] @(:selected?* widget))
          @(:widgets* s)))

(defn widget-bounds [widgets]
  (let [start-box (.getBounds (:affine (first widgets)))]
    (reduce (fn [box w]
              (.add box (.getBounds (:affine w)))
              box)
            start-box
            (next widgets))))

(defn selected-widget-bounds [s]
  (widget-bounds (selected-widgets s)))

(defn select-intersecting-widgets [s rect]
  (doseq [w (filter #(.intersects rect (.getBounds (:affine %)))
                    @(:widgets* s))]
    (select-widget s w)))

(defn surface-mode [s mode]
  (reset! (:mode* s) mode)
  (if (= :active mode)
    (deselect-all-widgets s)))

(defn surface-group [s]
  (println "group...")
  (try
    (let [bounds (bean (selected-widget-bounds s))
          border (sg/shape)
          widgets (selected-widgets s)
          group {:type :group
                 :widgets widgets
                 :border border}]
        (doto border
          (sg/anti-alias :on)
          (sg/mode :stroke)
          (sg/set-shape (sg/round-rectangle
                          (- (:x bounds) GROUP-PAD)
                          (- (:y bounds) GROUP-PAD)
                          (+ (:width bounds) GROUP-PAD)
                          (+ (:height bounds) GROUP-PAD) 5 5))
          (sg/stroke-color (color 255 255 255 120)))

        (sg/add (:group s) border)
        (swap! (:groups* s) group)
        (println "created group..."))
    (catch Exception e
      (println "Error: " e)
      (.printStackTrace e)))
  s)

(defn surface-ungroup
  [s])

(defn surface-duplicate [s]
  (println "duplicate...")
  (let [bounds (bean (selected-widget-bounds s))
        offset (+ (:width bounds) 10)
        widgets (selected-widgets)]
    ;(widget-forms widgets)
  ))

(comment defn- with-edit-bar [{:keys [mode* group width] :as s}]
  (let [background (sg/shape (sg/rectangle 0 0 width 30))
        bar-group  (sg/group)
        name-lbl   (sg/text "Name: ")
        name-tx    (sg/translate name-lbl 10 23)
        name-input (sg/text-input :columns 15)
        input-tx   (sg/translate name-input 65 5)
        x-lbl (sg/text "x: --")
        x-tx  (sg/translate x-lbl 300 23)
        y-lbl (sg/text "y: --")
        y-tx  (sg/translate y-lbl 370 23)]
    (doto background
      (sg/mode :fill)
      (sg/fill-color (darken (get-color :background) 1)))
    (doseq [node [name-lbl x-lbl y-lbl]]
      (doto node
        (sg/set-font "SansSerif" 12 :bold)
        (sg/mode :fill)
        (sg/fill-color (get-color :text))))
    (sg/add bar-group
            background
            name-tx input-tx
            x-tx y-tx)
    (sg/add group bar-group)

    (comment on-event ::widget-selected :edit-bar-handler
              (fn [{surface :surface {:keys [affine name]} :widget}]
                (let [{:keys [translateX translateY]} (bean (.getAffine affine))]
                  (println "edit-bar-handler: " name)
                  (sg/in-swing
                    (sg/set-text x-lbl (str "x: " translateX))
                    (sg/set-text y-lbl (str "y: " translateY))
                    (sg/set-text name-input name)))))

    (assoc s :edit-bar bar-group
           :name-input name-input)))

(defn widget-translate [w dx dy]
  (.transformBy (:affine w)
                (AffineTransform/getTranslateInstance (double dx)
                                                      (double dy))))


(defn surface-scale [s factor]
  (let [b1 (.getBounds (:affine s))
        _ (.transformBy (:affine s)
                        (AffineTransform/getScaleInstance factor factor))
        b2 (.getBounds (:affine s))]
    (widget-translate s
                      (/ (- (.getWidth b2) (.getWidth b1)) 2.0)
                      (/ (- (.getHeight b2) (.getHeight b1)) 2.0))))

(defn- setup-surface-mouse
  [surf]
  (let [selection-box (sg/shape)
        selection-rect (sg/rectangle 0 0 0 0)]

    (doto selection-box
      (sg/anti-alias :on)
      (sg/mode :stroke-fill)
      (sg/stroke-color (color 255 255 255 150))
      (sg/fill-color (color 255 255 255 50))
      (sg/set-shape selection-rect)
      (sg/visible false))
    (let [select-x* (atom 0)
          select-y* (atom 0)
          mouse-x* (atom 0)
          mouse-y* (atom 0)
          mode (:mode* surf)
          press-handler
          (fn [event]
            ; take the keyboard focus from the edit bar
            (.requestFocus (:group surf))

            ; deselect widgets unless shift
            (if (not (.isShiftDown event))
              (deselect-all-widgets surf))

            ; setup the selection box
            (when (= :edit @mode)
              (let [x (.getX event)
                    y (.getY event)]
                (sg/add (:group surf) selection-box)
                (.setRect selection-rect x y 1 1)
                (sg/set-shape selection-box selection-rect)
                (sg/visible selection-box true)
                (reset! select-x* x)
                (reset! select-y* y))))

          click-handler
          (fn [event]
            (when (= :edit @mode)
              (if (not (.isShiftDown event))
                (deselect-all-widgets surf))))

          drag-handler
          (fn [event]
            (when (= :edit @mode)
              (let [x (.getX event)
                    y (.getY event)
                    rect-x (min x @select-x*)
                    rect-y (min y @select-y*)
                    width (- (max x @select-x*) rect-x)
                    height (- (max y @select-y*) rect-y)]
                (.setRect selection-rect rect-x rect-y width height)
                (sg/set-shape selection-box selection-rect))))

          release-handler
          (fn [event]
            (when (= :edit @mode)
              (sg/visible selection-box false)
              (sg/remove (:group surf) selection-box)
              (select-intersecting-widgets surf selection-rect)))

          move-handler
          (fn [event]
            (reset! mouse-x* (.getX event))
            (reset! mouse-y* (.getY event)))]

      (sg/on-mouse (:group surf)
                   :click click-handler
                   :drag  drag-handler
                   :release release-handler
                   :press press-handler
                   :move move-handler)
      (assoc surf
             :mouse-x* mouse-x*
             :mouse-y* mouse-y*))))

(defn- center-shift [w]
  (let [b (.getBounds (:affine w))]
    [(/ (.getWidth b) 2.0) (/ (.getHeight b) 2.0)]))

(defn- widget-button [s key]
  (when-let [w-fn (get @widget-keys* key)]
    (let [{:keys [mouse-x* mouse-y* mode*]} s
          mouse-x @mouse-x*
          mouse-y @mouse-y*
          widget (w-fn s (keyword (gensym "widget")) 1 :x mouse-x :y mouse-y)
          shift (center-shift widget)]
      (println "shift: " shift)
      (apply widget-translate widget shift))))

;        widget (cond
;                 (= "B" key)
;                 ((:button @widget-fns*) s
;                    (keyword (gensym "button")) 1
;                    :x mouse-x :y mouse-y)
;
;                 (= "F" key)
;                 ((:fader @widget-fns*) s
;                    (keyword (gensym "fader")) 0.8
;                    :x mouse-x :y mouse-y)
;
;                 (= "D" key)
;                 ((:dial @widget-fns*) s
;                    (keyword (gensym "dial")) 0.5
;                    :x mouse-x :y mouse-y)
;
;                 (= "M" key)
;                 ((:monome @widget-fns*) s nil
;                    (keyword (gensym "monome"))
;                    :x mouse-x :y mouse-y)
;
;                 (= "L" key)
;                 ((:label @widget-fns*) s
;                    (keyword (gensym "label")) nil
;                    :x mouse-x :y mouse-y))

(defn- surface-key-press [surf key modifiers]
  (let [{:keys [mouse-x* mouse-y* mode*]} surf
        mouse-x @mouse-x*
        mouse-y @mouse-y*]
    (try
      (cond
        (and (= "Minus" key)       ; zoom out
             (= "Ctrl" modifiers))
        (surface-scale surf 0.9)

        (and (or (= "Equals" key)  ; zoom in
                 (= "Plus" key))
             (= "Ctrl" modifiers))
        (surface-scale surf 1.1)

        (and (= "Ctrl" modifiers) ; group
             (= "G" key))
        (surface-group surf)

        (and (= "Ctrl" modifiers) ; duplicate
             (= "D" key))
        (surface-duplicate surf)

        (= "E" key)
        (if (= :edit @mode*)
          (surface-mode surf :active)
          (surface-mode surf :edit))

        :else (widget-button surf key))

      (catch Exception e
        (println "Error: " e)
        (.printStackTrace e)))))

(defn- surface-center
  [s]
  {:x (/ (:width s) 2.0)
   :y (/ (:height s) 2.0)})

; TODO: finish me
(comment defn surface-save
  [s & [path]]
  (let [path (or path (file-chooser :type :save))]
    (if path nil)))

(declare surface-clear)

(defn surface-menu
  [s]
  (sg/menus
    [["Overtone"
      ["Save" #(println "save")]
      ["Quit" #(sg/visible (:frame s) false)]]
     ["Edit"
      ["Undo" #()]
      ["Redo" #()]
      ["Cut" #()]
      ["Copy" #()]
      ["Paste" #()]
      ["Duplicate" #(surface-duplicate s)]]
     ["Surface"
      ["Clear" #(surface-clear s)]
      ["Group" #(surface-group s)]
      ["Ungroup" #(surface-ungroup s)]
      ["Button" #((:button @widget-fns*) s (keyword (gensym "button")) 1)]
      ["Fader" #((:fader @widget-fns*) s (keyword (gensym "button")) 0.75)]
      ["Dial" #(((:dial @widget-fns*) s (keyword (gensym "dial")) 0.5))]]
     ["View"
      ["Zoom-in" #(surface-scale s 1.1)]
      ["Zoom-out" #(surface-scale s 0.9)]]]))

(defn surface
  [name width height]
  (let [frame (sg/frame name width height)
        panel (sg/panel width height)
        undo-man (sg/undo-manager)
        surf {:type :surface
              :name name
              :width width
              :height height
              :widgets* (atom #{})
              :groups* (atom #{})
              :undo undo-man}
        surf  (with-surface-group surf)
        affine (sg/affine (:group surf))
        mode* (atom :active)
        surf (assoc surf
                    :mode* mode*
                    :frame frame
                    :panel panel
                    :affine affine
                    :selected #{})
        surf (setup-surface-mouse surf)]

    (sg/set-scene panel affine)

    (sg/on-component frame
      :resize #(sg/set-shape (:background surf)
                             (sg/rectangle 0 0
                                           (.getWidth frame)
                                           (.getHeight frame))))

    (sg/on-key-pressed (:group surf)
      (fn [{:keys [key modifiers]}]
        (let [{:keys [mouse-x* mouse-y*]} surf
              mouse-x @mouse-x*
              mouse-y @mouse-y*]
              (println "key: " modifiers key)
              (println "mouse: " mouse-x "," mouse-y)
              (surface-key-press surf key modifiers))))

    (doto frame
      (.setJMenuBar (surface-menu surf))
      (.add panel)
      (.pack)
      (.setVisible true))

    surf))

(defn- next-widget-name [s w]
  (let [widgets (filter #(= (:type w) (:type %)) @(:widgets* s))]
    (str (name (:type w)) "-" (count widgets))))

(defn snap-lines [s]
  (let [bounds (map #(.getBounds (:affine %)) @(:widgets* s))
        xs (sort (flatten (map (fn [b]
                                 (let [{:keys [x centerX width]} (bean b)]
                                   [x centerX (+ x width)]))
                               bounds)))
        ys (sort (flatten (map (fn [b]
                                 (let [{:keys [y centerY height]} (bean b)]
                                   [y centerY (+ y height)]))
                               bounds)))]
    [xs ys]))

(defn snap-point-to-widgets [s x y]
  (let [[x-lines y-lines] (snap-lines s)
        x-line (first (filter #(< (Math/abs (- x %)) SNAP-DISTANCE) x-lines))
        y-line (first (filter #(< (Math/abs (- y %)) SNAP-DISTANCE) y-lines))]
    [(or x-line x) (or y-line y)]))

(defn surface-remove-widget
  "Remove a widget from the surface."
  [surface widget]
  (let [{:keys [group widgets]} surface
        {:keys [translate]} widget]
    (sg/remove group translate)
    (swap! widgets disj widget)

    surface))

(defn surface-clear [s]
  (doseq [w @(:widgets* s)]
    (surface-remove-widget s w)))

(defn surface-add-widget
  "Add a widget to the surface.

  The widget can have these optional properties:
    [name x y scale rotate label]

  For example:

  (surface-add-widget s (assoc (custom-widget)
    :x 10 :y 60 :label \"z-drag\"))
  "
  ([surface widget]
   (let [{:keys [name x y scale rotate label]} widget
         y (+ y EDIT-PADDING)
         wx x
         wy y
         {:keys [group widgets]} surface
         affine (sg/affine (:group widget))
         bounds (.getBounds (:group widget))
         lbl (label-group {})
         lbl-y (+ (.getHeight bounds) -5)
         lbl-tx (sg/translate (:group lbl) 0 lbl-y)
         w-bounds (.getBounds (:group widget))
         bounding-box (sg/shape)
         widget (assoc widget
                       :affine affine
                       :selected?* (atom false)
                       :bounding-box bounding-box)
         edit-mode? (= :edit @(:mode* surface))]

     (sg/add (:group widget) lbl-tx)

     (doto affine
       (.transformBy (AffineTransform/getTranslateInstance (double x) (double y)))
       (.transformBy (AffineTransform/getScaleInstance scale scale))
       (.transformBy (AffineTransform/getRotateInstance rotate))
       (sg/block-mouse true))

     ; Center the label below the widget
     (sg/observe (:value lbl)
       (fn [new-txt]
         (let [{:keys [x y width height]} (bean (.getBounds (:group lbl)))
               w-middle (/ (.getWidth w-bounds) 2)
               lbl-middle (/ width 2)
               lbl-x (double (- w-middle lbl-middle))]
           (.setTranslateX lbl-tx lbl-x))
         (println "after...")))

     (reset! (:value lbl) (or label (clojure.core/name name)))

     ;(.setToolTipText (:group widget) (or label (clojure.core/name name)))

     (doto bounding-box
       (sg/set-shape (sg/rectangle (.getX w-bounds)
                                   (.getY w-bounds)
                                   (.getWidth w-bounds)
                                   (.getHeight w-bounds)))
       (sg/mode :fill)
       (sg/fill-color (color 255 255 255 0))
       (sg/visible edit-mode?)
       (sg/block-mouse edit-mode?))

     (sg/add (:group widget) bounding-box)

     (sg/observe (:selected?* widget)
       (fn [selected?]
         (if selected?
           (sg/fill-color bounding-box (color 255 255 255 100))
           (sg/fill-color bounding-box (color 255 255 255 0)))))

     (sg/observe (:mode* surface)
       (fn [new-mode]
         (deselect-all-widgets surface)
         (let [status (= :edit new-mode)]
           (doto bounding-box
             (sg/visible status)
             (sg/block-mouse status)))))

     ; Set the label text
     (reset! (:value lbl) name)

     (let [last-x* (atom 0)
           last-y* (atom 0)

           press-handler
           (fn [event]
             ;(println "press: " x ", " y)
             (when (= :edit @(:mode* surface))
               (let [x (.getX event)
                     y (.getY event)]
                 ;(println "bounding press: " x ", " y)
                 (reset! last-x* x)
                 (reset! last-y* y))

               (cond
                 (.isShiftDown event)
                 (select-widget surface widget)

                 (not @(:selected?* widget))
                 (do
                   (deselect-all-widgets surface)
                   (select-widget surface widget)))))

           bounding-drag-handler
           (fn [event]
             (when (= :edit @(:mode* surface))
                 (let [cur-x (.getX event)
                       cur-y (.getY event)
                       dx (- cur-x @last-x*)
                       dy (- cur-y @last-y*)
                       bounds (selected-widget-bounds surface)
                       [dx dy] (if bounds
                                 (let [cx (+ dx (.getCenterX bounds))
                                       cy (+ dy (.getCenterY bounds))
                                       [snap-x snap-y] (snap-point-to-widgets surface cx cy)
                                       dx (+ dx (- snap-x cx))
                                       dy (+ dy (- snap-y cy))]
                                   [dx dy])
                                 [dx dy])]
                   (doseq [w (selected-widgets surface)]
                     (widget-translate w dx dy))
                   (reset! last-x* (+ @last-x* dx))
                   (reset! last-y* (+ @last-y* dy)))))]

       (sg/on-mouse bounding-box :press press-handler :drag bounding-drag-handler))

     (sg/add group affine)

     (swap! (:widgets* surface) conj widget)

     (sg/undoable (:undo surface) surface
                  "remove widget" #(surface-remove-widget surface widget)
                  "add widget" #(surface-add-widget surface widget))

     surface)))

;TODO: Turn mul, scale and rotate into observed atoms
(defn surface-register-widget
  "Takes a function that should accept an atom and return a scenegraph group representing the
  widget which is configured to update the atom whenever the value is changed."
  [w-name f w-key]
  (let [w-fn
        (fn [s name init-val & {:as options}]
          (let [center (surface-center s)
                options (merge center
                               {:scale 1 :mul 1 :rotate 0}
                               options)
                widget (f options)
                {:keys [x y scale mul]} options
                widget (assoc widget :name name :mul mul :x x :y y :scale scale :rotate 0)]
            (cond
              (number? init-val) (reset! (:value widget) (/ init-val (float mul)))
              (string? init-val) (reset! (:value widget) init-val))

            (surface-add-widget s widget)
            widget))]
    (swap! widget-fns* assoc w-name w-fn)
    (swap! widget-keys* assoc w-key w-fn)
    w-fn))

; Create the (label ...) function here so we can use label-group but not have a circular
; dependency with label.clj.
;(def label (widget-fn :label label-group))

(defn surface-vals [s]
  (into {} (map (fn [w] [(:name w) @(:value w)]) @(:widgets* s))))

(defn s-val [s name]
  (let [ws (filter #(= name (:name %)) @(:widgets* s))
        vals (map #(deref (:value %)) ws)]
    (if (= 1 (count vals))
      (first vals)
      vals)))

(defn surface-inst
  "Bind a surface to an instrument so any widgets with a name matching an instrument param name
  will send control messages to update the instrument in real-time."
  [surf inst-fn]
  (for [{:keys [name value mul]} @(:widgets* surf)]
      (add-watch value name
                 (fn [_ _ _ new-val]
                   (inst-fn :ctl name (* mul new-val))))))

(defn- widget-form
  [w]
  (let [{:keys [type name value x y mul]} w]
    (list (symbol (clojure.core/name type)) name @value :x x :y y)))

(defn surface-form
  ""
  [surf]
  (let [w-forms (doall (map widget-form @(:widgets* surf)))]
      (concat
        (list '-> (list 'surface (:name surf) (:width surf) (:height surf)))
        w-forms)))
