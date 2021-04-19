(ns dactyl-keyboard.hand
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.utils :refer [deg2rad]]))


;;;;;;;;;;
;; Hand ;;
;;;;;;;;;;

(defn finger [one two three finger-radius]
  (let
    [
      three-cyl-height (- three finger-radius)
      height-loss (* finger-radius (Math/sin 15))
      ]
    (union
     ;; First joint to second joint
     (translate [0 0 (/ one 2)]
                (cylinder finger-radius one))
     (translate [0 0 one]
                (rotate (deg2rad 15) [1 0 0]
                        (union
                         ;; Second joint to third
                         (translate [0 0 (/ two 2)]
                                    (cylinder finger-radius two))
                         ;; Third to end
                         (translate [0 (* -1 (- three-cyl-height height-loss) (Math/cos (deg2rad 75))) (+ two (/ three-cyl-height 2))]
                                    (rotate (deg2rad 15) [1 0 0]
                                            (union
                                             (cylinder finger-radius three-cyl-height)
                                             ;; Make the fingertip round
                                             (translate [0 0 (/ three-cyl-height 2)] (sphere finger-radius))))))
                        )
                )
     )
    )
  )

(def fingers
  ;; Move over by half the width of index finger to half index finger at 0 on x
  (translate [10.5 0 0]
             (union
              ;; Index
              (finger 47 22 20 10.5)
              ;; Middle
              (translate [25.5 0 0] (finger 53.5 29 22 9.2))
              ;; Ring
              (translate [(+ 20 25.5) 0 0] (finger 44 28.5 23 8.25))
              ;; Pinky
              (translate [(+ 20 25.5 22) 0 0] (finger 30 22.5 20 8.25))))
  )

(def palm
  (translate [42.5 0 -40] (union
                           (cube 85 30 80)
                           (rotate (deg2rad 35) [1 0 0]
                                   (translate [(+ 7 (/ -85 2)) -25 25]
                                              (cylinder 10.5 100)
                                              )
                                   )
                           )))

(def hand
  (union
   fingers
   (rotate (deg2rad -45) [1 0 0] palm)
   ))
