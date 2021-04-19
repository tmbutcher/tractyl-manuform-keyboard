(ns dactyl-keyboard.hotswap-mesh
  (:refer-clojure :exclude
                  [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.utils :refer [deg2rad]]
            [dactyl-keyboard.constants :refer :all]
            [dactyl-keyboard.placement :refer :all]
            [dactyl-keyboard.thumbs :refer :all]
            [dactyl-keyboard.utils :refer [deg2rad]]
            [dactyl-keyboard.hotswap :refer [official-hotswap-clamp]]
            [dactyl-keyboard.screws :refer [screw-insert-shape]]
            [dactyl-keyboard.dactyl :refer [model-right]]))

;;;;;;;;;;;;;
;; Hotswap ;;
;;;;;;;;;;;;;

(def hotswap-connector (translate [0 3 -2] (cube 2 6 2)))

(defn connector-place [column row hotswap hotswap-connector]
  (if (and
       (not= column lastcol)
       (not (and (= column 3) (= row lastrow))))
    (let [bottom          (key-place (+ 1 column) row (translate [-18 0 -8] hotswap-connector))
          bottom-next-row (key-place (+ 1 column) (+ row 1) (translate [-18 0 -8] hotswap-connector))]
      (union
       ; Hull directly down
       (hull (key-place column row hotswap-connector) bottom)
       ; hull over
       (hull bottom (key-place (+ 1 column) row hotswap-connector))
       ; hull to the next row
       (if (or (and (= row (- lastrow 1)) (= column 2))
               (< row (- lastrow 1)))
         (hull bottom bottom-next-row))))))

(defn thumb-hotswap-place [hotswap]
  (let [top-hotswap              (rotate (deg2rad 180) [0 0 1] hotswap)
        bottom-hotswap-connector (rotate (deg2rad 180) [0 0 1] hotswap-connector)]
    (union
     (thumb-mr-place (if trackball-enabled top-hotswap hotswap))
     (thumb-br-place hotswap)
     (if trackball-enabled nil (thumb-tl-place top-hotswap))
     (thumb-bl-place top-hotswap)
     (thumb-tr-place top-hotswap))))

(defn hotswap-place [hotswap]
  (let [top-hotswap              (rotate (deg2rad 180) [0 0 1] hotswap)
        bottom-hotswap-connector (rotate (deg2rad 180) [0 0 1] hotswap-connector)]
    (union
     ; top row is a litte different
     (apply union
            (for [column columns]
              (union
               (->> hotswap
                    (key-place column 0))
               (connector-place column 0 hotswap bottom-hotswap-connector))))
     (apply union
            (for [column columns
                  row    (range 1 nrows)
                  :when  (or (.contains [2 3] column)
                             (not= row lastrow))]
              (union
               (->> top-hotswap
                    (key-place column row))
               (connector-place column row top-hotswap hotswap-connector)))))))

(def hotswap-mesh
  (hotswap-place official-hotswap-clamp))

(def thumb-hotswap-mesh
  (thumb-hotswap-place official-hotswap-clamp))

(def hotswap-screw-hole (cylinder screw-insert-case-radius 10))

(defn hotswap-screw-place [in-shape]
  (let [shape (translate [0 -11.5 -2] in-shape)]
    (union
     (key-place 1 0 shape)
     (key-place 3 0 shape)
     (key-place 1 1 shape)
     (key-place 3 2 shape))))

(def hotswap-holes (hotswap-screw-place hotswap-screw-hole))
(def hotswap-screw-holders
  (let [shape      (rotate (deg2rad 180) [1 0 0]
                           (screw-insert-shape (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1.5)))
        hollow-out (rotate (deg2rad 180) [1 0 0]
                           (screw-insert-shape screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))]
    (difference
     (union
      (hotswap-screw-place shape)
      (hotswap-screw-place (hull shape (translate [-1.5 -10 2] (cube 11 2 3)))))
     (hotswap-screw-place hollow-out)
     (hotswap-screw-place hotswap-screw-hole))))

(def hotswap-mesh
  (difference
   (union
    hotswap-mesh
    hotswap-screw-holders)
   model-right))

(spit "things/hotswap-mesh.scad" (write-scad hotswap-mesh))
