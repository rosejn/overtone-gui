(ns overtone.gui
   (:require 
     [overtone.gui curve scope meter controller graph])
  (:use overtone.ns))

(immigrate
  'overtone.gui.curve
  'overtone.gui.scope
  'overtone.gui.meter
  'overtone.gui.controller
  'overtone.gui.graph)

