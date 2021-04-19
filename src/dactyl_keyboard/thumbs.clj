(ns dactyl-keyboard.thumbs
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.utils :refer [deg2rad]]
            [dactyl-keyboard.constants :refer :all]
            [dactyl-keyboard.web-connectors :refer :all]
            [dactyl-keyboard.placement :refer :all]))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(def thumborigin
  (map + (key-position 1 cornerrow [(+ (/ mount-width 2) 14) (+ (- (/ mount-height 3)) -1) 2])
       thumb-offsets))
(def thumb-tip-origin (map + thumborigin thumb-tip-offset))


;(def thumborigin
;  (map + (key-position 1 cornerrow [(+ (/ mount-width 2) 25) (+ (- (/ mount-height 3)) 0) 8])
;       thumb-offsets))

(defn thumb-tr-place [shape]
  (->> shape
       (rotate (deg2rad  -7) [1 0 0])
       (rotate (deg2rad -45) [0 1 0])
       (rotate (deg2rad  27) [0 0 1]) ; original 10
       (translate thumborigin)
       (translate [-21 -12.5 11]))) ; original 1.5u  (translate [-12 -16 3])

(def tl-thumb-loc (map + thumb-tip-offset (if trackball-enabled trackball-middle-translate [0 0 0])))
(defn thumb-tl-place [shape]
  (->> shape
       (rotate (deg2rad  -12) [1 0 0])
       (rotate (deg2rad -54) [0 1 0])
       (rotate (deg2rad  35) [0 0 1]) ; original 10
       (translate thumborigin)
       (translate tl-thumb-loc))) ; original 1.5u (translate [-32 -15 -2])))

(def mr-thumb-loc (map + [-23.5 -36.5 -2] (if trackball-enabled trackball-middle-translate [0 0 0])))
(defn thumb-mr-place [shape]
  (->> shape
       (rotate (deg2rad  -12) [1 0 0])
       (rotate (deg2rad -54) [0 1 0])
       (rotate (deg2rad  35) [0 0 1])
       (translate thumborigin)
       (translate mr-thumb-loc)))

(def br-thumb-loc (map + [-34.5 -44 -20] (if trackball-enabled [2 -12 2] [0 0 0])))
(defn thumb-br-place [shape]
  (->> shape
       (rotate (deg2rad   -18) [1 0 0])
       (rotate (deg2rad -55) [0 1 0])
       (rotate (deg2rad  37) [0 0 1])
       (translate thumborigin)
       (translate br-thumb-loc)))

(def bl-thumb-loc (map + [-44 -23 -24] (if trackball-enabled [2 -12 2] [0 0 0])))
(defn thumb-bl-place [shape]
  (->> shape
       (rotate (deg2rad   -18) [1 0 0])
       (rotate (deg2rad -55) [0 1 0])
       (rotate (deg2rad  37) [0 0 1])
       (translate thumborigin)
       (translate bl-thumb-loc))) ;        (translate [-51 -25 -12])))


(defn thumb-1x-layout [shape]
  (union
   (thumb-mr-place shape)
   (thumb-br-place shape)
   (if trackball-enabled nil (thumb-tl-place shape))
   (thumb-bl-place shape)))

(defn thumb-15x-layout [shape]
  (union
   (thumb-tr-place shape)))


(def thumb
  (union
   (thumb-1x-layout single-plate)
   (thumb-15x-layout single-plate)
   ))

(def thumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
(def thumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
(def thumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def thumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post))

(def thumb-connectors
  (if trackball-enabled
    (union
     ; top right vertical
     (triangle-hulls
      (thumb-tr-place web-post-br)
      (thumb-tr-place web-post-bl)
      (thumb-mr-place web-post-br))
     ; Between the top and middle
     (triangle-hulls
      (thumb-tr-place web-post-tl)
      (thumb-mr-place web-post-tr)
      (thumb-mr-place web-post-br))
     (triangle-hulls
      (thumb-tr-place web-post-bl)
      (thumb-tr-place web-post-tl)
      (thumb-mr-place web-post-br))
     ; Between middle and first bottom
     (triangle-hulls
      (thumb-mr-place web-post-tl)
      (thumb-br-place web-post-tr)
      (thumb-br-place web-post-br))
     (triangle-hulls
      (thumb-mr-place web-post-bl)
      (thumb-mr-place web-post-tl)
      (thumb-br-place web-post-br)
      (thumb-bl-place web-post-br))
     ; Between the top and middle over by the trackball
     (triangle-hulls
      (thumb-tr-place web-post-tl)
      (thumb-mr-place web-post-tr)
      (thumb-mr-place web-post-tl))
     ; Between the bottom two
     (triangle-hulls
      (thumb-br-place web-post-tr)
      (thumb-br-place web-post-tl)
      (thumb-bl-place web-post-br))
     (triangle-hulls
      (thumb-bl-place web-post-br)
      (thumb-bl-place web-post-bl)
      (thumb-br-place web-post-tl))
     ; Between the middle and the bl
     (triangle-hulls
      (thumb-mr-place web-post-tl)
      (thumb-bl-place web-post-tr)
      (thumb-bl-place web-post-br))
     (triangle-hulls    ; top two to the main keyboard, starting on the left
      (key-place 0 cornerrow web-post-br)
      (thumb-tr-place thumb-post-tl)
      (key-place 1 cornerrow web-post-bl)
      (thumb-tr-place thumb-post-tr)
      (key-place 1 cornerrow web-post-br)
      (key-place 2 lastrow web-post-tl)
      (key-place 2 lastrow web-post-bl)
      (thumb-tr-place thumb-post-tr)
      (key-place 2 lastrow web-post-bl)
      (thumb-tr-place thumb-post-br)
      (key-place 2 lastrow web-post-br)
      (key-place 3 lastrow web-post-bl)
      (key-place 2 lastrow web-post-tr)
      (key-place 3 lastrow web-post-tl)
      (key-place 3 cornerrow web-post-bl)
      (key-place 3 lastrow web-post-tr)
      (key-place 3 cornerrow web-post-br)
      (key-place 4 cornerrow web-post-bl))
     (triangle-hulls
      (key-place 1 cornerrow web-post-br)
      (key-place 2 lastrow web-post-tl)
      (key-place 2 cornerrow web-post-bl)
      (key-place 2 lastrow web-post-tr)
      (key-place 2 cornerrow web-post-br)
      (key-place 3 cornerrow web-post-bl))
     (triangle-hulls
      (key-place 3 lastrow web-post-tr)
      (key-place 3 lastrow web-post-br)
      (key-place 3 lastrow web-post-tr)
      (key-place 4 cornerrow web-post-bl)))
    (union
     (triangle-hulls    ; top two
      (thumb-tl-place web-post-tr)
      (thumb-tl-place web-post-br)
      (thumb-tr-place thumb-post-tl)
      (thumb-tr-place thumb-post-bl))
     (triangle-hulls    ; bottom two
      (thumb-br-place web-post-tr)
      (thumb-br-place web-post-br)
      (thumb-mr-place web-post-tl)
      (thumb-mr-place web-post-bl))
     (triangle-hulls
      (thumb-mr-place web-post-tr)
      (thumb-mr-place web-post-br)
      (thumb-tr-place thumb-post-br))
     (triangle-hulls    ; between top row and bottom row
      (thumb-br-place web-post-tl)
      (thumb-bl-place web-post-bl)
      (thumb-br-place web-post-tr)
      (thumb-bl-place web-post-br)
      (thumb-mr-place web-post-tl)
      (thumb-tl-place web-post-bl)
      (thumb-mr-place web-post-tr)
      (thumb-tl-place web-post-br)
      (thumb-tr-place web-post-bl)
      (thumb-mr-place web-post-tr)
      (thumb-tr-place web-post-br))
     (triangle-hulls    ; top two to the middle two, starting on the left
      (thumb-tl-place web-post-tl)
      (thumb-bl-place web-post-tr)
      (thumb-tl-place web-post-bl)
      (thumb-bl-place web-post-br)
      (thumb-mr-place web-post-tr)
      (thumb-tl-place web-post-bl)
      (thumb-tl-place web-post-br)
      (thumb-mr-place web-post-tr))
     (triangle-hulls    ; top two to the main keyboard, starting on the left
      (thumb-tl-place web-post-tl)
      (key-place 0 cornerrow web-post-bl)
      (thumb-tl-place web-post-tr)
      (key-place 0 cornerrow web-post-br)
      (thumb-tr-place thumb-post-tl)
      (key-place 1 cornerrow web-post-bl)
      (thumb-tr-place thumb-post-tr)
      (key-place 1 cornerrow web-post-br)
      (key-place 2 lastrow web-post-tl)
      (key-place 2 lastrow web-post-bl)
      (thumb-tr-place thumb-post-tr)
      (key-place 2 lastrow web-post-bl)
      (thumb-tr-place thumb-post-br)
      (key-place 2 lastrow web-post-br)
      (key-place 3 lastrow web-post-bl)
      (key-place 2 lastrow web-post-tr)
      (key-place 3 lastrow web-post-tl)
      (key-place 3 cornerrow web-post-bl)
      (key-place 3 lastrow web-post-tr)
      (key-place 3 cornerrow web-post-br)
      (key-place 4 cornerrow web-post-bl))
     (triangle-hulls
      (key-place 1 cornerrow web-post-br)
      (key-place 2 lastrow web-post-tl)
      (key-place 2 cornerrow web-post-bl)
      (key-place 2 lastrow web-post-tr)
      (key-place 2 cornerrow web-post-br)
      (key-place 3 cornerrow web-post-bl))
     (triangle-hulls
      (key-place 3 lastrow web-post-tr)
      (key-place 3 lastrow web-post-br)
      (key-place 3 lastrow web-post-tr)
      (key-place 4 cornerrow web-post-bl)))
    )
  )
