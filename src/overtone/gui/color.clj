(ns overtone.gui.color
  (:import
    (java.awt EventQueue Dimension Point Dimension Color Font
              RenderingHints Point BasicStroke BorderLayout FlowLayout)
    (javax.swing JFrame JPanel JLabel JButton SwingUtilities
                 JSpinner SpinnerNumberModel JColorChooser
                 BoxLayout JTextArea JScrollPane JTable)
    (javax.swing.event ChangeListener)
    (javax.swing.table AbstractTableModel))
  (:use (overtone event log)
        (overtone.gui swing)))

(def color* (ref
              {:fill-1 (Color. 0 130 226 150)
               :stroke-1 (Color. 0 140 236)
               :fill-2 (Color. 170 170 170 150)
               :stroke-2 (Color. 170 170 170)
               :fill-3 (Color.  0 130 236 150)
               :stroke-3 (Color. 0 130 236)
               :background (Color. 40 40 40)
               :grid-lines (Color. 80 80 80)
               :current-color (Color. 0 130 226)
               :highlight (Color. 100 100 255)
               :bounding-box (Color. 250 20 20 150)

               :node-bg (Color. 10 10 10)
               :text (Color. 200 200 200)

               :button-stroke (Color. 255 50 50)
               :button-fill   (Color. 255 50 50 120)
               }))

(defn get-color [tag]
  (get @color* tag (:current-color @color*)))

(defn color 
  ([r g b] (Color. r g b))
  ([r g b a] (Color. r g b a)))

(defn transparent-color
  "Create a fill color from a corresponding stroke color."
  [col]
  (let [dark (.darker col)
        r (.getRed dark)
        g (.getGreen dark)
        b (.getBlue dark)]
    (Color. r g b 150)))

(def color-handler* (ref nil))

(defn stop-color []
  (remove-handler :color-changed @color-handler*)
  (dosync (ref-set color-handler* nil)))

(defn live-color [handler]
  (stop-color)
  (dosync (ref-set color-handler* handler))
  (on-event :color-changed :live-color-handler #(handler (:color %))))

(defn color-panel []
  (let [color-chooser (JColorChooser. )
        choosers (.getChooserPanels color-chooser)
        preview (JPanel.)]
    (doto color-chooser
      (.setBackground (get-color :background))
      ;(.setForeground (color :foreground))
      (.setChooserPanels (into-array [(nth choosers 1)]))
      (.setPreviewPanel preview)
      )

    ;(.add (.getContentPane color-chooser) preview)

    (comment doseq [cp choosers]
      (doto cp
        (.setBackground (get-color :background))
        (.setForeground (get-color :foreground)))
      (doseq [comp (seq (.getComponents cp))]
        (.setBackground comp (get-color :background))))

    (-> color-chooser
      (.getSelectionModel)
      (.addChangeListener
        (proxy [ChangeListener] []
          (stateChanged [_] (event :color-changed
                                   :color (.getColor color-chooser))))))
    color-chooser))

(defn color-frame []
  (let [f (JFrame. "Color Picker")]
    (doto f
      ;(.setPreferredSize (Dimension. 400 400))
      (.add (color-panel))
      (.pack)
      (.show))))
