(ns dactyl-keyboard.utils
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.model :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))