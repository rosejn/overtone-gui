(ns overtone.gui.sg
  ^{:doc "A gui library using swing and a 2D scenegraph for Overtone."
     :author "Fabian Aussems & Jeff Rose"}
  (:refer-clojure :exclude [remove])
  (:use [overtone util event]
        [clojure.contrib.str-utils2 :only (capitalize)])
  (:import
    (clojure.lang RT)
    (com.sun.scenario.scenegraph
     JSGPanel ProportionalPaint SGAbstractGeometry SGAbstractShape
     SGAbstractShape$Mode SGAlignment SGArc SGCircle SGClip SGComponent
     SGComposite SGCubicCurve SGEffect SGEllipse SGEmbeddedToolkit
     SGFilter SGGroup SGImage SGImageOp SGLeaf SGLine SGNode SGParent
     SGPerspective SGQuadCurve SGRectangle SGRenderCache SGShape
     SGSourceContent SGText SGTransform SGTransform$Affine
     SGTransform$Rotate SGTransform$Scale SGTransform$Shear
     SGTransform$Translate SGWrapper)
   (com.sun.scenario.scenegraph.event
     SGFocusListener SGNodeListener SGNodeEvent
     SGKeyListener SGMouseListener)
   (com.sun.scenario.effect
     AbstractGaussian Blend Bloom Brightpass ColorAdjust DropShadow
     Effect GaussianBlur Identity Merge Offset PhongLighting SepiaTone
     Shadow Glow Source SourceContent Blend$Mode Effect$AccelType)
   (com.sun.scenario.animation Clip Timeline)
   (com.sun.scenario.effect.light
     DistantLight Light PointLight SpotLight Light$Type )
   (java.awt BasicStroke BorderLayout Color Point Dimension
             Font Insets RenderingHints Shape 
             Toolkit GraphicsEnvironment)
   (java.awt.event KeyEvent MouseEvent MouseListener MouseAdapter)
   (java.awt.geom Point2D$Double Line2D$Double Path2D$Double
                  CubicCurve2D$Double QuadCurve2D$Double
                  Rectangle2D$Double RoundRectangle2D$Double
                  Arc2D Arc2D$Double Ellipse2D$Double AffineTransform)
   (java.awt.image.BufferedImage)
   (javax.swing SwingUtilities JComponent JFrame JPanel JTabbedPane
                JTextField JLabel ImageIcon JSlider)
   (javax.swing.border.EmptyBorder)
   (javax.swing.event ChangeEvent ChangeListener)))

(def DEFAULT-INPUT-COLS 12)

(defmacro in-swing [& body]
  `(SwingUtilities/invokeLater (fn [] ~@body)))

(defn observe [ref handler]
  (add-watch ref (gensym "sg/watch")
    (fn [_ _ _ new-val]
      (in-swing (run-handler handler new-val)))))

(defn screen-dim []
  (.getScreenSize (Toolkit/getDefaultToolkit)))

(defn screen-size []
  (let [dim (screen-dim)]
    [(.width dim) (.height dim)]))

;TODO: Fix me
; It undecorates, but it doesn't seem to change the size of the frame...
(defn fullscreen-frame [f]
    (.setExtendedState f JFrame/MAXIMIZED_BOTH)
    (.setUndecorated f true))

(defn frame
  ([name]
   (JFrame. name))
  ([name width height]
   (doto (JFrame. name)
     (.setPreferredSize (Dimension. width height)))))

(defn panel
  ([] (doto (JSGPanel.)
        (.setBackground java.awt.Color/GRAY)))
  ([w h] (doto (panel)
           (.setPreferredSize (java.awt.Dimension. w h)))))

(defn set-scene [panel scene]
  (.setScene panel scene))

(defn add
  "Add nodes to a scenegraph group."
  [group & nodes]
  (doseq [node nodes]
    (.add group node)))

(defn remove
  "Remove nodes from a scenegraph group."
  [group & nodes]
  (doseq [node nodes]
    (.remove group node)))

(defn group
  "Create a scenegraph group node."
  []
  (SGGroup.))

(defn set-shape
  "Set the 2D shape of a scenegraph shape node."
  [node shape]
  (.setShape node shape))

(defn block-mouse
  "Set true if you want to stop mouse events from propagating below this node.
  NOTE: must be a filled shape."
  [node blocked?]
  (.setMouseBlocker node blocked?))

(defn visible
  "Turn a node on or off."
  [node val]
  (.setVisible node val))

(defn shape
  "Create a scenegraph shape node."
  ([] (SGShape.))
  ([path]
   (let [s (SGShape.)]
     (set-shape s path)
     s)))

(defn set-text
  "Set the text string on a text node."
  [node text]
  (.setText node text))

(defn text
  "Create a text scenegraph node."
  ([] (text ""))
  ([txt]
   (doto (SGText.)
     (set-text txt))))

(defn text-input
  "Create a text input node."
  ([] (text-input {}))
  ([& {:as options}]
   (let [text (get options :text "")
         columns (get options :columns DEFAULT-INPUT-COLS)]
     (doto (SGComponent.)
       (.setComponent (JTextField. text columns))))))

(defn fonts
  "Returns the names of all available fonts."
  []
  (seq (.getAvailableFontFamilyNames
    (GraphicsEnvironment/getLocalGraphicsEnvironment))))

(def FONT-STYLES
  {:plain Font/PLAIN
   :bold  Font/BOLD
   :italic Font/ITALIC})

(defn font
  ([name] (font name 12 :plain))
  ([name size] (font name size :plain))
  ([name size style]
   (let [style (if (keyword? style)
                 (style FONT-STYLES)
                 style)]
   (Font. name style size))))

(defn set-font
  "Set the font for a text node."
  ([node name]
   (.setFont node name))
  ([node name size]
   (.setFont node (font name size)))
  ([node name size style]
   (.setFont node (font name size style))))

(defn font-size
  [node size]
  (let [{:keys [name style]} (bean (.getFont node))]
    (set-font node (font name size style))))

(defn point [x y]
  (Point2D$Double. x y))

(defn line [x1 y1 x2 y2]
  (Line2D$Double. x1 y1 x2 y2))

(defn path []
  (Path2D$Double.))

(defn line-to [p x y]
  (.lineTo p x y))

(defn move-to [p x y]
  (.moveTo p x y))

;; CubicCurve2D$Double QuadCurve2D$Double

(def ARC-TYPES
  {:chord Arc2D/CHORD
   :open  Arc2D/OPEN
   :pie   Arc2D/PIE})

(defn arc
  ([x y width height start-angle extent]
    (arc x y width height start-angle extent :open))
  ([x y width height start-angle extent close-type]
   (Arc2D$Double. x y width height
                  start-angle extent
                  (get ARC-TYPES close-type))))

(defn ellipse [x y width height]
  (Ellipse2D$Double. x y width height))

(defn round-rectangle [x y width height corner-width corner-height]
  (RoundRectangle2D$Double. x y width height corner-width corner-height))

(defn rectangle [x y width height]
  (Rectangle2D$Double. x y width height))

;; Transformations applied to a group affect all children

(defn rotate
  [node theta]
  (SGTransform$Rotate/createRotation theta node))

(defn scale
  [node sx sy]
  (SGTransform$Scale/createScale sx sy node ))

(defn shear
  [node shx shy]
  (SGTransform$Shear/createShear shx shy node ))

(defn translate
  [node tx ty]
  (SGTransform$Translate/createTranslation tx ty node))

(defn affine
  [node]
  (SGTransform$Affine/createAffine (AffineTransform.) node))

(defn animation
  [node length property start end]
  (Clip/create (long length) 1 node
               property (to-array [start end])))

(defn animate
  [& anims]
  (if (= 1 (count anims))
    (.start anims)
    (let [timeline (Timeline.)]
      (doseq [a anims]
        (.schedule timeline a))
      (.start timeline))))

(def shape-mode-map {:fill        SGAbstractShape$Mode/FILL
                     :stroke      SGAbstractShape$Mode/STROKE
                     :stroke-fill SGAbstractShape$Mode/STROKE_FILL})

(defn mode
  "Set the draw mode for this mode, one of :fill, :stroke, or :stroke-fill."
  [node mode]
  (.setMode node (shape-mode-map mode)))

(def stroke-cap-map  {:butt   BasicStroke/CAP_BUTT
                      :round  BasicStroke/CAP_ROUND
                      :square BasicStroke/CAP_SQUARE})

(def stroke-join-map {:bevel  BasicStroke/JOIN_BEVEL
                      :miter  BasicStroke/JOIN_MITER
                      :round  BasicStroke/JOIN_ROUND})

(defn stroke-style
  ([node width] (.setDrawStroke node (BasicStroke. width)))
  ([node width cap join] (.setDrawStroke node (BasicStroke. width (stroke-cap-map cap) (stroke-join-map join)))))

(def color-map {:black      Color/BLACK
                :blue       Color/BLUE
                :cyan       Color/CYAN
                :dark-gray  Color/DARK_GRAY
                :gray       Color/GRAY
                :green      Color/GREEN
                :light-gray Color/LIGHT_GRAY
                :magenta    Color/MAGENTA
                :orange     Color/ORANGE
                :pink       Color/PINK
                :red        Color/RED
                :white      Color/WHITE
                :yellow     Color/YELLOW})

(defn color
  ([r g b] (Color. r g b))
  ([r g b a] (Color. r g b a)))

(defn stroke-color
  ([node color]
   (if (= java.awt.Color (type color))
     (.setDrawPaint node color)
     (.setDrawPaint node (color-map color))))
  ([node r g b] (.setDrawPaint node (Color. r g b)))
  ([node r g b a] (.setDrawPaint node (Color. r g b a))))

(defn fill-color
  ([node color]
   (cond
     (= java.awt.Color (type color)) (.setFillPaint node color)
     (keyword? color) (.setFillPaint node (color-map color))))
  ([node r g b] (.setFillPaint node (Color. r g b)))
  ([node r g b a] (.setFillPaint node (Color. r g b a))))

(defn set-width! [node w] (.setWidth node w))
(defn set-height! [node h] (.setHeight node h))
(defn set-arc-width! [node w] (.setArcWidth node w))
(defn set-arc-height! [node h] (.setArcWidth node h))
(defn set-center-x! [node cx] (.setCenterX node cx))
(defn set-center-y! [node cy] (.setCenterX node cy))
(defn set-radius! [node r] (.setRadius node r))
(defn set-radius-x! [node r] (.setRadiusX node r))
(defn set-radius-y! [node r] (.setRadiusY node r))
(defn set-x! [node x] (.setX node x))
(defn set-y! [node y] (.setY node y))
(defn set-x1! [node x1] (.setX1 node x1))
(defn set-y1! [node y1] (.setY1 node y1))
(defn set-x2! [node x2] (.setX2 node x2))
(defn set-y2! [node y2] (.setY2 node y2))
(defn set-ctrl-x! [node ctrlx] (.setCtrlX node ctrlx))
(defn set-ctrl-y! [node ctrly] (.setCtrlY node ctrly))
(defn set-ctrl-x1! [node ctrlx] (.setCtrlX1 node ctrlx))
(defn set-ctrl-y1! [node ctrly] (.setCtrlY1 node ctrly))
(defn set-ctrl-x2! [node ctrlx] (.setCtrlX2 node ctrlx))
(defn set-ctrl-y2! [node ctrly] (.setCtrlY2 node ctrly))

(defn sg-circle
  ([] (SGCircle.))
  ([cx cy r] (doto (sg-circle)
               (set-center-x! cx)
               (set-center-y! cy)
               (set-radius! r))))

(defn sg-cubic-curve
  ([] (SGCubicCurve.))
  ([x1 y1 x2 y2 ctrlx1 ctrly1 ctrlx2 ctrly2] (doto (sg-cubic-curve)
                                               (set-x1! x1)
                                               (set-y1! y1)
                                               (set-x2! x2)
                                               (set-y2! y2)
                                               (set-ctrl-x1! ctrlx1)
                                               (set-ctrl-y1! ctrly1)
                                               (set-ctrl-x2! ctrlx2)
                                               (set-ctrl-y2! ctrly2))))

(defn sg-ellipse
  ([] (SGEllipse.))
  ([cx cy rx ry] (doto (sg-ellipse)
                   (set-center-x! cx)
                   (set-center-y! cy)
                   (set-radius-x! rx)
                   (set-radius-y! ry))))

(defn sg-line
  ([] (SGLine.))
  ([x1 y1 x2 y2] (doto (SGLine.)
                   (set-x1! x1)
                   (set-y1! y1)
                   (set-x2! x2)
                   (set-y2! y2))))

(defn sg-quad-curve
  ([] (SGQuadCurve.))
  ([x1 y1 x2 y2 ctrlx ctrly] (doto (sg-quad-curve)
                               (set-x1! x1)
                               (set-y1! y1)
                               (set-x2! x2)
                               (set-y2! y2)
                               (set-ctrl-x! ctrlx)
                               (set-ctrl-y! ctrly))))

(defn sg-rectangle
  ([] (SGRectangle.))
  ([w h] (doto (sg-rectangle)
           (set-width! w)
           (set-height! h)))
  ([x y w h] (doto (sg-rectangle)
               (set-x! x)
               (set-y! y)
               (set-width! w)
               (set-height! h)))
  ([x y w h arcw arch] (doto (sg-rectangle)
                         (set-x! x)
                         (set-y! y)
                         (set-width! w)
                         (set-height! h)
                         (set-arc-width! arcw)
                         (set-arc-height! arch))))

(def antialias-map {:default RenderingHints/VALUE_ANTIALIAS_DEFAULT
                    :off     RenderingHints/VALUE_ANTIALIAS_OFF
                    :on      RenderingHints/VALUE_ANTIALIAS_ON})

(defn anti-alias [node value] (.setAntialiasingHint node (antialias-map value)))

(defn set-location [node x y]
  (.setLocation node (Point2D$Double. x y)))

(def text-antialias-map {:default  RenderingHints/VALUE_TEXT_ANTIALIAS_DEFAULT
                         :gasp     RenderingHints/VALUE_TEXT_ANTIALIAS_GASP
                         :lcd-hbgr RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR
                         :lcd-hrgb RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HRGB
                         :lcd-vbgr RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_VBGR
                         :lcd-vrgb RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_VRGB
                         :off      RenderingHints/VALUE_TEXT_ANTIALIAS_OFF
                         :on       RenderingHints/VALUE_TEXT_ANTIALIAS_ON})

(defn set-text-antialias! [node val] (.setAntialiasingHint node (text-antialias-map val)))

(defmacro on-property-change [node [event] & body]
  `(. ~node addPropertyChangeListener
      (proxy [java.beans.PropertyChangeListener] []
        (propertyChange [~event] ~@body))))


(def effect-map {:phong-lighting        PhongLighting
                 :glow                  Glow
                 :shadow                Shadow})

(def blend-mode-map {:add           Blend$Mode/ADD
                     :blue          Blend$Mode/BLUE
                     :color-burn    Blend$Mode/COLOR_BURN
                     :color-dodge   Blend$Mode/COLOR_DODGE
                     :darken        Blend$Mode/DARKEN
                     :difference    Blend$Mode/DIFFERENCE
                     :exclusion     Blend$Mode/EXCLUSION
                     :green         Blend$Mode/GREEN
                     :hard-light    Blend$Mode/HARD_LIGHT
                     :lighten       Blend$Mode/LIGHTEN
                     :multiply      Blend$Mode/MULTIPLY
                     :overlay       Blend$Mode/OVERLAY
                     :red           Blend$Mode/RED
                     :screen        Blend$Mode/SCREEN
                     :soft-light    Blend$Mode/SOFT_LIGHT
                     :src-atop      Blend$Mode/SRC_ATOP
                     :src-in        Blend$Mode/SRC_IN
                     :src-out       Blend$Mode/SRC_OUT
                     :src-over      Blend$Mode/SRC_OVER})

(def light-type-map {:distant Light$Type/DISTANT
                     :point   Light$Type/POINT
                     :spot    Light$Type/SPOT})

(defmacro set-prop! [node kw val]
  `( ~(symbol (str ".set" (capitalize (name kw)))) ~node ~val))

; (macroexpand-1 `(set-prop! glow :level 0.1))

(defmacro create-fx [fx-name args]
  `(new ~(fx-name effect-map) ~@args ))

;(macroexpand-1 `(create-fx :shadow [:a :b :c]))

(defmacro fx
  ([fx-name args] `(create-fx ~fx-name ~args))
  ([fx-name args fx & body]
      `(let [~fx (create-fx ~fx-name ~args)]
         ~@body
         ~fx)))

(defn set-fx! [effect fx] (.setEffect effect fx))
(defn set-child! [effect node] (.setChild effect node) )

(defn sg-effect
  ([] (SGEffect.))
  ([effect] (doto (sg-effect) (set-fx! effect)))
  ([effect child] (doto (sg-effect)
                    (set-fx! effect)
                    (set-child! child))))

;(println (macroexpand-1 `(fx :shadow [] fx (println fx))))
;(effect :glow [] fx (doto fx (.setLevel 0.4)))

(defn component
  ([] (SGComponent.))
  ([comp]
     (doto (component)
       (.setComponent comp)))
  ([comp w h]
     (doto (component comp)
       (.setSize w h))))

(defn on-focus-gained
  "on focus gained event"
  [node handler]
  (.addFocusListener node
     (reify SGFocusListener
       (focusGained [this event node]
         (run-handler handler event node))
       (focusLost [this event node]))))

(defn on-focus-lost
  "on focus lost event"
  [node handler]
  (.addFocusListener node
      (reify SGFocusListener
        (focusGained [this event node])
        (focusLost [this event node]
          (run-handler handler event node)))))

(defn key-event [e]
  {:key (KeyEvent/getKeyText (.getKeyCode e))
   :modifiers (KeyEvent/getKeyModifiersText (.getModifiers e))})

(defn on-key-pressed [node handler]
  (.addKeyListener node
      (reify SGKeyListener
        (keyPressed [this event node]
                    (run-handler handler (key-event event) node))
        (keyReleased [this event node])
        (keyTyped [this event node] ))))

(defn on-key-released [node handler]
  (.addKeyListener node
      (reify SGKeyListener
        (keyPressed [this event node])
        (keyReleased [this event node]
                     (run-handler handler (key-event event) node))
        (keyTyped [this event node] ))))

(defn on-key-typed [node handler]
  (.addKeyListener node
      (proxy [SGKeyListener] []
        (keyPressed [this event node])
        (keyReleased [this event node])
        (keyTyped [this event node]
          (run-handler handler event node)))))

(defn- handle [handlers key event node]
  (if-let [handler (get handlers key)]
    (run-handler handler event node)))

(defn on-mouse [node & {:as handlers}]
  (.addMouseListener node
    (reify SGMouseListener
      (mouseDragged [this event node]
        (handle handlers :drag event node))
      (mousePressed [this event node]
        (handle handlers :press event node))
      (mouseReleased [this event node]
        (handle handlers :release event node))
      (mouseClicked [this event node]
        (handle handlers :click event node))
      (mouseEntered [this event node]
        (handle handlers :enter event node))
      (mouseExited [this event node]
        (handle handlers :exit event node))
      (mouseMoved [this event node]
        (handle handlers :move event node))
      (mouseWheelMoved [this event node]
        (handle handlers :wheel event node)))))

(defn on-bounds-changed [node handler]
  (.addNodeListener node
      (reify SGNodeListener
        (boundsChanged [this event] (run-handler handler event)))))

