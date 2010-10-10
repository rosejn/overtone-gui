(ns overtone.gui.graph
  (:import
     (java.awt Dimension Color Font RenderingHints Point BasicStroke)
     (java.awt.geom Ellipse2D$Float RoundRectangle2D$Float Rectangle2D$Float
                    Arc2D$Float)
     (javax.swing JFrame JPanel)
     (com.sun.scenario.scenegraph JSGPanel SGText SGShape SGGroup SGAbstractShape$Mode)
     (com.sun.scenario.scenegraph.event SGMouseAdapter)
     (com.sun.scenario.scenegraph.fx FXShape)
     (com.sun.scenario.animation Clip Interpolators)
     (com.sun.scenario.effect DropShadow))
  (:use overtone.live
        [vijual :only (tree-to-shapes layout-tree idtree image-dim)]
        [clojure.contrib.string :only (trim)]
        [overtone.gui sg color]))

(def NODE-HEIGHT 20)
(def NODE-ARC 4)
(def NODE-PAD-X 10)
(def NODE-PAD-Y 20)
(def NODE-FONT-SIZE 12)

(defn ugen-label [sdef ugen]
  (let [u-name (:name ugen)
        args (repeat (count (:inputs ugen)) "arg ")
        text (apply str u-name " " args)
        lbl (SGText.)]
    (doto lbl
      (.setText text)
      (.setFont (Font. "SansSerif" Font/BOLD NODE-FONT-SIZE))
      (.setAntialiasingHint RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
      (.setFillPaint (color :text)))))

(defn ugen-mouse-listener [glow anim]
  (proxy [SGMouseAdapter] []
    (mouseEntered [evt node] (.start anim))
    (mouseExited  [evt node]
                 (.stop anim)
                 (.setRadius glow 0))))

(defn ugen-node-view
  "Create a node object representing the given ugen in sdef."
  [sdef ugen x y]
  (let [box (FXShape.)
        text (ugen-label sdef ugen)
        bounds (.getBounds text)
        group (SGGroup.)
        glow (DropShadow.)
        clip (Clip/create (long 2000) Clip/INDEFINITE glow "radius" (to-array [(float 1.0) (float 15.0)]))
        listener (ugen-mouse-listener glow clip)]
    (.setLocation text (Point. (+ x NODE-PAD-X) (+ y NODE-PAD-Y (.height bounds))))

    (doto glow
      (.setRadius 1.0)
      (.setColor (Color. 88 248 246)))

    (doto box
      (.setShape (RoundRectangle2D$Float. x y
                                          (+ (* 2 NODE-PAD-X) (.width bounds))
                                          (+ (* 2 NODE-PAD-Y) (.height bounds))
                                          NODE-ARC NODE-ARC))
      (.setMode SGAbstractShape$Mode/STROKE_FILL)
      (.setAntialiasingHint RenderingHints/VALUE_ANTIALIAS_ON)
      (.setFillPaint (color :fill-1))
      (.setDrawPaint (color :stroke-1))
      (.setDrawStroke (BasicStroke. 1.15)))

    (.setEffect box glow)

    (doto clip
      (.setInterpolator (Interpolators/getLinearInstance)))

    (doto group
      (.add box)
      (.add text)
      (.addMouseListener listener))

    group))

(defn position-ugen-nodes [nodes])

(def VIEW-PADDING 10)
(def NODE-MARGIN 20)
(def ROW-HEIGHT 30)

(def sdef-group (ref nil))

(defn ugen-node [ugen sdef]
  (let [consts (:constants sdef)
        ugens (:ugens sdef)
        args (map (fn [input]
                    (cond
                      (= -1 (:src input)) (nth consts (:index input))
                      :default (nth ugens (:index input))))
                  (:inputs ugen))]))

(defn sdef-graph
  "Convert an sdef into some more easily dealt with tree structure so we
  can"
  [sdef]
  (let [ugens (reverse (:ugens sdef))]))

(defn sdef-view [sdef]
  (let [group (SGGroup.)]
    (.add group (ugen-node-view sdef (first (:ugens sdef)) 100 100))
;        ugen-nodes (loop [x 0
;                          y 0
;                          nodes {}
;                          ugens (reverse (:ugens sdef))]
;                     (if ugens
;                       (let [ugen (first ugens)
;                             id (:id ugen)
;                             [x y] (if (contains? nodes id)
;                                     (+ y ROW-HEIGHT)
;                                     y)
;                             node (ugen-node-view sdef ugen x y)
;                             width (-> node (.getBounds) (.width))]
;                         (recur (+ x width NODE-MARGIN)
;                                y
;                                (assoc nodes id node)
;                                (next ugens)))
;                       nodes))]
;    ;(position-nodes nodes)
;    (doseq [[id n] ugen-nodes]
;      (.add group n))
;    ;(dosync (ref-set sdef-group group))
    group))


(defn label [text]
  (let [args (repeat (count (:inputs ugen)) "arg ")
        text (apply str text " " args)
        lbl (SGText.)]
    (doto lbl
      (.setText text)
      (.setFont (Font. "SansSerif" Font/BOLD NODE-FONT-SIZE))
      (.setAntialiasingHint RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
      (.setFillPaint (color :text)))))

(def BUTTON-WIDTH 20)
(def BUTTON-HEIGHT 18)

(defn- synth-kill-button [ids]
  (let [button (FXShape.)]
    (doto button
      (.setShape (Arc2D$Float. 0 0 BUTTON-WIDTH BUTTON-HEIGHT
                               180 180 Arc2D$Float/CHORD))
      (.setAntialiasingHint RenderingHints/VALUE_ANTIALIAS_ON)
      ;(set-mode! :stroke)
      (.setDrawPaint (color :button-stroke))
      (.setDrawStroke (BasicStroke. 2.0))
      (set-mode! :fill)
      (.setFillPaint (color :button-fill)))

    (on-mouse-clicked button #(apply kill ids))
    button))

(defn node-shape
  [{:keys [type x y width height text] :as shape}]
  (let [text (trim (second text))
        group (SGGroup.)
        box (FXShape.)
        lbl (label text)
        bounds (.getBounds lbl)]
    (doto box
      (.setShape (RoundRectangle2D$Float. 0 0 width height NODE-ARC NODE-ARC))
      (set-mode! :stroke-fill)
      (.setAntialiasingHint RenderingHints/VALUE_ANTIALIAS_ON)
      (.setFillPaint (color :node-bg))
      (.setDrawPaint (color :stroke-1))
      (.setDrawStroke (BasicStroke. 1.15)))

    (set-location! lbl NODE-PAD-X NODE-PAD-Y)
    (add! group box lbl)

    (if (.startsWith text "ids:")
      (let [button (synth-kill-button (map #(Integer/parseInt %)
                                          (re-seq #"[0-9]+" text)))
            btn-x (- (/ width 2) (/ BUTTON-WIDTH 2))
            btn-y (- height (/ BUTTON-HEIGHT 2))]
      (add! group (translate button btn-x btn-y))))

    (translate group x y)))

(defn edge-shape
  [{:keys [type x y width height text] :as shape}]
  (let [box (FXShape.)]
    (doto box
      (.setShape (Rectangle2D$Float. 0 0 width height))
      (.setAntialiasingHint RenderingHints/VALUE_ANTIALIAS_ON)
      (set-mode! :fill)
      (.setFillPaint (color :highlight)))
    (translate box x y)))

(def dimensions
  (merge image-dim
         {:line-wid 1
          :node-padding 15
          :line-padding 15}))

(defn- draw-shapes-scene []
  (let [tree [(vijual-tree (node-tree))]
        shapes (tree-to-shapes dimensions (layout-tree dimensions (idtree tree)))
        scene (SGGroup.)
        grouped-shapes (group-by
                         (fn [{:keys [type text]}]
                           (if (= type :rect)
                            (cond
                              (nil? text) :edge
                              :else :node)
                            nil))
                         shapes)]
    (doseq [{:keys [type text] :as edge} (:edge grouped-shapes)]
      (add! scene (edge-shape edge)))

    (doseq [{:keys [type text] :as node} (:node grouped-shapes)]
      (add! scene (node-shape node)))

    (translate scene 100 100)))

(defn node-tree-frame []
  (let [g-frame (JFrame.  "Synth Tree")
        g-panel (JSGPanel.)
        update-scene (fn [& _] (.setScene g-panel (draw-shapes-scene)))]
    (.add (.getContentPane g-frame) g-panel)

    (doto g-panel
      (.setBackground (color :background))
      (.setScene (draw-shapes-scene))
      (.setPreferredSize (Dimension. 500 500)))

    (on-event "/n_go" :node-tree-create update-scene)
    (on-event "/n_end" :node-tree-destroy update-scene)

    (doto g-frame
      (.add g-panel)
      (.pack)
      (.setVisible true))))

(definst foo
  [freq 440]
  (* 0.01 (saw [freq (* 0.99 freq)])))

(defn graph-window [sdef]
  (let [g-frame (JFrame.  "Project Overtone: Graph View")
        g-panel (JSGPanel.)]
    (.add (.getContentPane g-frame) g-panel)

    (doto g-panel
      (.setBackground (color :background))
      (.setScene (sdef-view sdef))
      (.setPreferredSize (Dimension. 1000 1000)))

    (doto g-frame
      (.add g-panel)
      (.pack)
      (.setVisible true))))
