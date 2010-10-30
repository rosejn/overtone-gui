(ns
  ^{:doc "A monome control"
    :author "Jeff Rose"}
  overtone.gui.surface.monome
  (:use
    clojure.stacktrace
    [overtone event]
    [overtone.gui color])
  (:require [overtone.gui.sg :as sg]))

(def MONOME-BUTTON-SIZE 25)
(def MONOME-MARGIN 6)
(def FMONOME-BUTTON-SIZE (+ MONOME-BUTTON-SIZE MONOME-MARGIN))
(def MONOME-CORNER 3)

(defn m-button [monome x y]
  (let [{:keys [color handler]} monome
        group  (sg/group)
        back   (sg/shape)
        box    (sg/shape)
        x-pos  (+ (* x FMONOME-BUTTON-SIZE) MONOME-MARGIN)
        y-pos  (+ (* y FMONOME-BUTTON-SIZE) MONOME-MARGIN)
        status (atom false)
        press-handler (fn [event]
                        (swap! status not)
                        (if handler
                          (handler x y @status))
                        (if @status
                          (sg/fill-color back (transparent-color @color))
                          (sg/fill-color back (get-color :background)))
                        (println "status: " @status))]
    (doto back
      (sg/mode :fill)
      (sg/fill-color (get-color :background))
      (sg/set-shape (sg/round-rectangle x-pos y-pos 
                                  MONOME-BUTTON-SIZE MONOME-BUTTON-SIZE
                                  MONOME-CORNER MONOME-CORNER)))

    (add-watch color (gensym "monome-color")
               (fn [_ _ _ new-color]
                 (if @status
                   (sg/fill-color back (fill-color new-color))
                   (sg/fill-color back (get-color :background)))
                 (sg/stroke-color box new-color)))

    (doto box
      (sg/anti-alias :on)
      (sg/mode :stroke)
      (sg/stroke-color @color)
      (sg/set-shape (sg/round-rectangle x-pos y-pos
                                       MONOME-BUTTON-SIZE MONOME-BUTTON-SIZE
                                       MONOME-CORNER MONOME-CORNER)))
    (sg/add group 
            back box)

    (sg/on-mouse-pressed back press-handler)
    (sg/on-mouse-pressed box press-handler)
    group))

(defn monome
  ([columns rows] (monome columns rows false))
  ([columns rows handler]
   (let [width   (+ MONOME-MARGIN (* columns FMONOME-BUTTON-SIZE))
         height  (+ MONOME-MARGIN (* rows FMONOME-BUTTON-SIZE))
         group   (sg/group)
         border  (sg/shape)
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
       (sg/mode :stroke)
       (sg/stroke-color @m-color)
       (sg/set-shape (sg/round-rectangle 0 0 width height
                                        MONOME-CORNER
                                        MONOME-CORNER)))
     (apply sg/add group border buttons)

     mono)))
