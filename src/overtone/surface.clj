(ns overtone.surface
   (:require
     [overtone.gui.surface core button dial fader label mixer monome properties])
  (:use overtone.ns))

(immigrate
  'overtone.gui.surface.core
  'overtone.gui.surface.button
  'overtone.gui.surface.dial
  'overtone.gui.surface.fader
  'overtone.gui.surface.label
  'overtone.gui.surface.mixer
  'overtone.gui.surface.monome
  'overtone.gui.surface.properties)

