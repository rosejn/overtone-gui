(ns overtone.gui.demo
  (:import java.awt.Color)
  (:use [overtone.gui surface button monome fader dial color]))

(defn add-mixer-channel [s x y]
  (let [vol (fader)
        hi  (dial)
        mid (dial)
        low (dial)
        cut (button)]
    (doseq [d [hi mid low]]
      (reset! (:color d) (Color. 0 200 0)))
    (reset! (:color cut) (Color. 200 0 0))
    (surface-add-widget s vol x y)
    (surface-add-widget s hi  (+ 30 x) (+ 10 y))
    (surface-add-widget s mid (+ 30 x) (+ 60 y))
    (surface-add-widget s low (+ 30 x) (+ 110 y))
    (surface-add-widget s cut x (+ 150 y))
    s))

(defn test-surface []
  (let [s (surface "Test Surface" 500 500)]
    (dotimes [i 4]
      (add-mixer-channel s (+ 10 (* i 80)) 10))
    (surface-add-widget s (monome 8 8) 10 200)
    s))
