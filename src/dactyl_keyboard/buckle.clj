(ns dactyl-keyboard.buckle
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.utils :refer [deg2rad]]))


(defn buckle [& {:keys [triangle-length triangle-width buckle-width-adjust buckle-width buckle-thickness buckle-length buckle-end-length buckle-height include-middle end-supports?] :or [end-supports? true]}]
  (let
    [buckle-end-width (- buckle-width (* 2 buckle-thickness))
     palm-buckle-triangle (polygon [[0 triangle-length] [triangle-width 0] [0 0]])
     palm-buckle-side (translate [0 (- (+ buckle-length buckle-end-length))]
                                 (square buckle-thickness (+ buckle-length buckle-end-length) :center false))
     palm-buckle-2d (union
                     ; Triangles
                     (translate [(/ buckle-width 2) 0 0] palm-buckle-triangle)
                     (translate [(- (/ buckle-width 2)) 0 0]
                                (mirror [1 0] palm-buckle-triangle))
                     ; Sticks on the triangles
                     (translate [(/ buckle-width 2) 0 0] palm-buckle-side)
                     (translate [(- (/ buckle-width 2)) 0 0]
                                (mirror [1 0] palm-buckle-side))
                     (if include-middle
                       (union
                        ; Square in the middle
                        (translate [0 (- (+ buckle-length (/ buckle-end-length 2)))]
                                   (square buckle-end-width buckle-end-length))
                        ; Bar at the end
                        (translate [0 (- (+ buckle-length buckle-end-length (/ buckle-thickness 2)))]
                                   (square (+ buckle-width (* 2 buckle-thickness)) buckle-thickness)))
                       nil))]
    (extrude-linear { :height buckle-height } palm-buckle-2d)))

(defn buckle-holes [& {:keys [buckle-thickness buckle-length buckle-width buckle-width-adjust triangle-length triangle-width buckle-height]}]
  (let [hole-x-translate (- (/ (+ buckle-width buckle-width-adjust) 2) (- triangle-width buckle-thickness) 0.2)]
    (union
     (translate [hole-x-translate 0 0]
                (cube (+ triangle-width 0.5) 10 (+ buckle-height 0.5) :center false))
     (translate [(+ hole-x-translate (- triangle-width buckle-thickness)) buckle-length 0] ; clear out some space on the other end of the buckle
                (cube (+ triangle-width 0.25) 2 (+ buckle-height 0.5) :center false))
     (translate [(- hole-x-translate) 0 0]
                (mirror [1 0] (cube (+ triangle-width 0.5) 10 (+ buckle-height 0.5) :center false)))
     (translate [(- (- hole-x-translate) (- triangle-width buckle-thickness)) buckle-length 0] ;clear out some space on the other end of the buckle
                (mirror [1 0] (cube (+ triangle-width 0.25) 2 (+ buckle-height 0.5) :center false))))))