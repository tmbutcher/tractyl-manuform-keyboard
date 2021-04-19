(ns dactyl-keyboard.screws
  (:refer-clojure :exclude
                  [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.utils :refer [deg2rad]]
            [dactyl-keyboard.constants :refer :all]
            [dactyl-keyboard.walls :refer :all]
            [dactyl-keyboard.placement :refer :all]))


;;;;;;;;;;;;
;; Screws ;;
;;;;;;;;;;;;
(defn screw-insert-shape [bottom-radius top-radius height]
  (union
   (->>
     (binding [*fn* 30]
       (cylinder [bottom-radius top-radius] height)))
   (translate [0 0 (/ height 2)] (->> (binding [*fn* 30] (sphere top-radius))))))


(defn screw-insert [column row bottom-radius top-radius height offset]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up
                        (key-position column row (map + (wall-locate2 0 1) [0 (/ mount-height 2) 0]))
                        (if shift-down
                          (key-position column row (map - (wall-locate2 0 -1) [0 (/ mount-height 2) 0]))
                          (if shift-left
                            (map + (left-key-position row 0) (wall-locate3 -1 0))
                            (key-position column row (map + (wall-locate2 1 0) [(/ mount-width 2) 0 0])))))]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate (map + offset [(first position) (second position) (/ height 2)])))))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union (screw-insert 0 0 bottom-radius top-radius height [7.5 7 0])
         (screw-insert 0 lastrow bottom-radius top-radius height (if trackball-enabled [-2 33 0] [0 15 0]))
         ;  (screw-insert lastcol lastrow  bottom-radius top-radius height [-5 13 0])
         ;  (screw-insert lastcol 0         bottom-radius top-radius height [-3 6 0])
         (screw-insert lastcol lastrow bottom-radius top-radius height [-3.5 17 0])
         (screw-insert lastcol 0 bottom-radius top-radius height [-1 2 0])
         (screw-insert 1 lastrow bottom-radius top-radius height (if trackball-enabled [1 -16 0] [1 -18.5 0]))))

(def screw-insert-holes
  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))

(spit "things/screw-test.scad"
      (write-scad
       (difference
        (screw-insert 0 0 (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1.5) [0 0 0])
        (screw-insert 0 0 screw-insert-bottom-radius screw-insert-top-radius screw-insert-height [0 0 0]))))

; Wall Thickness W:\t1.65
(def screw-insert-outers
  (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1.5)))
(def screw-insert-screw-holes
  (screw-insert-all-shapes screw-insert-case-radius screw-insert-case-radius 350))