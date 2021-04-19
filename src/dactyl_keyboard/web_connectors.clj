(ns dactyl-keyboard.web-connectors
  (:refer-clojure :exclude
                  [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.utils :refer [deg2rad]]
            [dactyl-keyboard.constants
             :refer
             [plate-thickness mount-width mount-height pinky-15u]]))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 2)
(def post-size 0.1)
(def web-post
  (->> (cube post-size post-size web-thickness)
       (translate
         [0
          0
          (+ (/ web-thickness -2)
             plate-thickness)])))

(def big-boi-web-post
  (->> (cube post-size (+ post-size 30) 10)
       (translate
         [0
          0
          (-
            (+ (/ 10 -2)
               plate-thickness)
            1)])))

(def post-adj (/ post-size 2))
(def web-post-tr
  (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl
  (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl
  (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br
  (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))


; wide posts for 1.5u keys in the main cluster

(if (true? pinky-15u)
  (do
    (def wide-post-tr
      (translate
        [(- (/ mount-width 1.2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
    (def wide-post-tl
      (translate
        [(+ (/ mount-width -1.2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
    (def wide-post-bl
      (translate
        [(+ (/ mount-width -1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
    (def wide-post-br
      (translate
        [(- (/ mount-width 1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post)))
  (do (def wide-post-tr web-post-tr)
    (def wide-post-tl web-post-tl)
    (def wide-post-bl web-post-bl)
    (def wide-post-br web-post-br)))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))