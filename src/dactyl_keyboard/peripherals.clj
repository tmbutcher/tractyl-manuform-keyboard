(ns dactyl-keyboard.peripherals
  (:refer-clojure :exclude
                  [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.utils :refer [deg2rad]]
            [dactyl-keyboard.placement :refer :all]
            [dactyl-keyboard.walls :refer :all]
            [dactyl-keyboard.constants :refer :all]))

(def usb-holder-ref
  (key-position 0 0 (map - (wall-locate2 0 -1) [0 (/ mount-height 2) 0])))

(def usb-holder-position
  (map + [5 16 0] [(first usb-holder-ref) (second usb-holder-ref) 2]))
(def usb-holder-cube (cube 18.5 35 4))
(def usb-holder-holder
  (translate (map + usb-holder-position [5 -12.9 0])
             (difference (cube 21 39 6) (translate [0 0 1] usb-holder-cube))))

(def usb-jack
  (translate (map + usb-holder-position [5 10 4])
             (union
              (translate [0 -2.5 0] (cube 12 3 5))
              (cube 6.5 11.5 3.1))))

(def pro-micro-position (map + (key-position 0 1 (wall-locate3 -1 0)) [-6 2 -15]))
(def pro-micro-space-size [4 10 12])

; z has no wall;
(def pro-micro-wall-thickness 2)
(def pro-micro-holder-size [(+ pro-micro-wall-thickness (first pro-micro-space-size))
                            (+ pro-micro-wall-thickness (second pro-micro-space-size))
                            (last pro-micro-space-size)])
(def pro-micro-space
  (->>
    (cube (first pro-micro-space-size) (second pro-micro-space-size) (last pro-micro-space-size))
    (translate
      [(- (first pro-micro-position) (/ pro-micro-wall-thickness 2))
       (- (second pro-micro-position) (/ pro-micro-wall-thickness 2))
       (last pro-micro-position)])))
(def pro-micro-holder
  (difference
   (->>
     (cube (first pro-micro-holder-size) (second pro-micro-holder-size) (last pro-micro-holder-size))
     (translate
       [(first pro-micro-position)
        (second pro-micro-position)
        (last pro-micro-position)]))
   pro-micro-space))

(def trrs-holder-size [6.2 10 3])

; trrs jack PJ-320A
(def trrs-holder-hole-size [6.2 10 6])

; trrs jack PJ-320A
(def trrs-holder-position (map + usb-holder-position [20.5 0 0]))
(def trrs-holder-thickness 2)
(def trrs-holder-thickness-2x (* 2 trrs-holder-thickness))
(def trrs-holder
  (union
   (->>
     (cube (+ (first trrs-holder-size) trrs-holder-thickness-2x)
           (+ trrs-holder-thickness (second trrs-holder-size))
           (+ (last trrs-holder-size) trrs-holder-thickness))
     (translate
       [(first trrs-holder-position)
        (second trrs-holder-position)
        (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2)]))))
(def trrs-holder-hole
  (union

   ; circle trrs hole
   (->>
    (->> (binding [*fn* 30] (cylinder 3.3 20))) ; 5mm trrs jack
    (rotate (deg2rad 90) [1 0 0])
    (translate
      [(first trrs-holder-position)
       (+ (second trrs-holder-position)
          (/ (+ (second trrs-holder-size) trrs-holder-thickness) 2))
       (+ 3 (/ (+ (last trrs-holder-size) trrs-holder-thickness) 0.9))]))
   ;1.5 padding

   ; rectangular trrs holder
   (->> (apply cube trrs-holder-hole-size)
        (translate
          [(first trrs-holder-position)
           (+ (/ trrs-holder-thickness -2) (second trrs-holder-position))
           (+ (/ (last trrs-holder-hole-size) 2) trrs-holder-thickness)]))))
