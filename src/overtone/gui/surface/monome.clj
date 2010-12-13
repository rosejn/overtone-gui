(ns
  ^{:doc "A monome control"
    :author "Jeff Rose"}
  overtone.gui.surface.monome
  (:use
    clojure.stacktrace
    [overtone event]
    [overtone.gui color]
    [overtone.gui.surface core])
  (:require [overtone.gui.sg :as sg]))

(def MONOME-BUTTON-SIZE 25)
(def MONOME-MARGIN 6)
(def FMONOME-BUTTON-SIZE (+ MONOME-BUTTON-SIZE MONOME-MARGIN))
(def MONOME-CORNER 3)

(defn- toggle-status [vals x y]
  (let [cur-status (get-in vals [x y])]
    (assoc-in vals [x y] (not cur-status))))

(defn m-button [monome x y]
  (let [{:keys [value color]} monome
        group  (sg/group)
        back   (sg/shape)
        box    (sg/shape)
        x-pos  (+ (* x FMONOME-BUTTON-SIZE) MONOME-MARGIN)
        y-pos  (+ (* y FMONOME-BUTTON-SIZE) MONOME-MARGIN)
        press-handler (fn [event]
                        (swap! value toggle-status x y)
                        (if (get-in @value [x y])
                          (sg/fill-color back (transparent-color @color))
                          (sg/fill-color back (get-color :background))))]
    (doto back
      (sg/mode :fill)
      (sg/fill-color (get-color :background))
      (sg/set-shape (sg/round-rectangle x-pos y-pos
                                  MONOME-BUTTON-SIZE MONOME-BUTTON-SIZE
                                  MONOME-CORNER MONOME-CORNER)))

    (sg/observe color
      (fn [new-color]
        (if (get-in @value [x y])
          (sg/fill-color back (transparent-color new-color))
          (sg/fill-color back (get-color :background)))
        (sg/stroke-color box new-color)))

    (doto box
      (sg/anti-alias :on)
      (sg/mode :stroke)
      (sg/stroke-color @color)
      (sg/set-shape (sg/round-rectangle x-pos y-pos
                                       MONOME-BUTTON-SIZE MONOME-BUTTON-SIZE
                                       MONOME-CORNER MONOME-CORNER)))
    (sg/add group back box)
    (sg/on-mouse group :press press-handler)

    {:type :m-button
     :group group
     :x x
     :y y}))

(defn- monome*
  [{:keys [rows columns color value]}]
  (let [width   (+ MONOME-MARGIN (* columns FMONOME-BUTTON-SIZE))
        height  (+ MONOME-MARGIN (* rows FMONOME-BUTTON-SIZE))
        group   (sg/group)
        border  (sg/shape)
        m-color (atom (or color (get-color :stroke-1)))
        mono    {:type :monome
                 :group group
                 :columns columns
                 :rows rows
                 :color m-color
                 :value value}
        m-vals (vec (repeat rows
                            (vec (repeat columns false))))
        buttons (doall (for [i (range columns)
                             j (range rows)]
                         (m-button mono i j)))]
    (doto border
      (sg/mode :stroke)
      (sg/stroke-color @m-color)
      (sg/set-shape (sg/round-rectangle 0 0 width height
                                        MONOME-CORNER
                                        MONOME-CORNER)))
    (apply sg/add group border (map :group buttons))

    (assoc mono :buttons buttons)))

(def monome (widget-fn monome*))

(defn monome-led-on [x y])
(defn monome-led-off [x y])

