(ns overtone.gui.surface.mixer
  (:use
    clojure.stacktrace
    [overtone.gui color]
    [overtone.gui.surface core button monome fader dial])
  (:require [overtone.gui.sg :as sg]))

(defn add-mixer-channel [s x y]
  (let [vol (fader)
        hi  (dial)
        mid (dial)
        low (dial)
        cut (button)]
    (doseq [d [hi mid low]]
      (reset! (:color d) (get-color :stroke-2)))
    (reset! (:color cut) (get-color :stroke-3))
    (surface-add-widget s vol x y)
    (surface-add-widget s hi  (+ 30 x) (+ 10 y))
    (surface-add-widget s mid (+ 30 x) (+ 60 y))
    (surface-add-widget s low (+ 30 x) (+ 110 y))
    (surface-add-widget s cut x (+ 150 y))
    s))

(def MIXER-WIDTH 550)
(def MIXER-HEIGHT 230)

(defn flip-coin []
  (zero? (rand-int 2)))

(defn add-button [s]
  (let [{:keys [width height]} s
        x (rand-int width)
        y -10
        cx (* 0.5 width)
        cy (* 0.5 height)
        btn (surface-add-widget s (button) x y)
        anim-x (sg/animation (:translate btn) 300 "TranslateX" x cx)
        anim-y (sg/animation (:translate btn) 300 "TranslateY" y cy)]
    (println "x: " x "y: " y)
    (println "cx: " cx "cy: " cy)
    (sg/animate anim-x anim-y)))

(defn mixer
  ([] (mixer MIXER-WIDTH MIXER-HEIGHT))
  ([width height]
   (let [s (surface "Mixer" width height)]
     (dotimes [i 4]
       (add-mixer-channel s (+ 20 (* i 80)) 10))
     (surface-add-widget s (monome 4 4) 350 20)
     (sg/on-key-pressed (:group s)
       (fn [{:keys [key modifiers]}]
        (println "key: " key modifiers)
         (cond
           (= "B" key) (add-button s))))
     ;(doto (:frame s)
     ;  (.repaint))
     s)))

(defn change-color [widget color] 
  (println "color: " color)
  (reset! (:color widget) color))

(defn widgets-of-type [m w-type]
  (filter #(= w-type (:type %)) @(:widgets m)))

(defn set-widget-color [m w-type color]
  (try
    (doseq [w (widgets-of-type m w-type)]
      (change-color w color))
    (catch Exception e
      (println "exception in set-widget-color: " e)
      (println (.printStackTrace e)))))

;(on-event :color-changed :mixer-color-change (fn [event] (set-widget-color m :button (:color event))))
