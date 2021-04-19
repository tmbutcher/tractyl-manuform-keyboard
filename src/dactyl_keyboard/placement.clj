(ns dactyl-keyboard.placement
  (:refer-clojure :exclude
                  [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.utils :refer [deg2rad]]
            [dactyl-keyboard.constants :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius
  (+
    (/ (/ (+ mount-height extra-height) 2)
       (Math/sin (/ α 2)))
    cap-top-height))
(def column-radius
  (+
    (/ (/ (+ mount-width extra-width) 2)
       (Math/sin (/ β 2)))
    cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))

(defn offset-for-column [col]
  (if (and (true? pinky-15u) (= col lastcol)) 5.5 0))
(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [column-angle       (* β (- centercol column))
        placed-shape       (->> shape
                                (translate-fn [(offset-for-column column) 0 (- row-radius)])
                                (rotate-x-fn (* α (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                (translate-fn [0 0 (- column-radius)])
                                (rotate-y-fn column-angle)
                                (translate-fn [0 0 column-radius])
                                (translate-fn (column-offset column)))
        column-z-delta     (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- row-radius)])
                                (rotate-x-fn (* α (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                (rotate-y-fn column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))
        placed-shape-fixed (->> shape
                                (rotate-y-fn (nth fixed-angles column))
                                (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                (translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
                                (rotate-x-fn (* α (- centerrow row)))
                                (translate-fn [0 0 (+ row-radius (nth fixed-z column))])
                                (rotate-y-fn fixed-tenting)
                                (translate-fn [0 (second (column-offset column)) 0]))]
    (->>
      (case column-style
        :orthographic placed-shape-ortho
        :fixed        placed-shape-fixed
        placed-shape)
      (rotate-y-fn tenting-angle)
      (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle) (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle) 0 (Math/sin angle)]
    [0 1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))


(def single-plate
  (let [top-wall     (->> (cube (+ keyswitch-width 3) 1.5 plate-thickness)
                          (translate
                            [0
                             (+ (/ 1.5 2) (/ keyswitch-height 2))
                             (/ plate-thickness 2)]))
        left-wall    (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                          (translate
                            [(+ (/ 1.5 2) (/ keyswitch-width 2))
                             0
                             (/ plate-thickness 2)]))
        side-nub     (->> (binding [*fn* 30] (cylinder 1 2.75))
                          (rotate (/ π 2) [1 0 0])
                          (translate [(+ (/ keyswitch-width 2)) 0 1])
                          (hull
                            (->> (cube 1.5 2.75 side-nub-thickness)
                                 (translate
                                   [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                    0
                                    (/ side-nub-thickness 2)])))
                          (translate [0 0 (- plate-thickness side-nub-thickness)]))
        plate-half   (union top-wall left-wall (if create-side-nubs? (with-fn 100 side-nub)))
        top-nub      (->> (cube 5 5 retention-tab-hole-thickness)
                          (translate [(+ (/ keyswitch-width 2)) 0 (/ retention-tab-hole-thickness 2)]))
        top-nub-pair (union top-nub
                            (->> top-nub
                                 (mirror [1 0 0])
                                 (mirror [0 1 0])))]
    (difference
     (union plate-half
            (->> plate-half
                 (mirror [1 0 0])
                 (mirror [0 1 0])))
     (if create-top-nubs?
       (->>
        top-nub-pair
        (rotate (/ π 2) [0 0 1]))
       nil))))

(def key-holes
  (apply union
         (for [column columns
               row    rows
               :when  (or (.contains [2 3] column)
                          (not= row lastrow))]
           (->> single-plate
                (key-place column row)))))

(defn left-key-position [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0])
       [left-wall-x-offset 0 left-wall-z-offset]))

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))
