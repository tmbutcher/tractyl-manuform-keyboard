(ns dactyl-keyboard.walls
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.utils :refer [deg2rad]]
            [dactyl-keyboard.constants :refer :all]
            [dactyl-keyboard.thumbs :refer :all]
            [dactyl-keyboard.placement :refer :all]
            [dactyl-keyboard.web-connectors :refer :all]))

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))


(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])

(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
   (hull
    (place1 post1)
    (place1 (translate (wall-locate1 dx1 dy1) post1))
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 post2)
    (place2 (translate (wall-locate1 dx2 dy2) post2))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))
   (bottom-hull
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))

(def right-wall
  (let [tr (if (true? pinky-15u) wide-post-tr web-post-tr)
        br (if (true? pinky-15u) wide-post-br web-post-br)]
    (union (key-wall-brace lastcol 0 0 1 tr lastcol 0 1 0 tr)
           (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 tr lastcol y 1 0 br))
           (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 br lastcol y 1 0 tr))
           (key-wall-brace lastcol cornerrow 0 -1 br lastcol cornerrow 1 0 br))))

(def thumb-to-left-wall (union
                         ; clunky bit on the top left thumb connection  (normal connectors don't work well)
                         (bottom-hull
                          (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
                          (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
                          (thumb-bl-place (translate (wall-locate2 -2 1) web-post-tr))
                          (thumb-bl-place (translate (wall-locate3 -3 1) web-post-tr)))
                         (hull
                          (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
                          (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
                          (thumb-bl-place (translate (wall-locate2 -2 1) web-post-tr))
                          (thumb-bl-place (translate (wall-locate3 -2 1) web-post-tr))
                          (thumb-tl-place web-post-tl))
                         (hull
                          (left-key-place cornerrow -1 web-post)
                          (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
                          (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
                          (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
                          (thumb-tl-place web-post-tl))
                         (hull
                          (left-key-place cornerrow -1 web-post)
                          (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
                          (key-place 0 cornerrow web-post-bl)
                          (thumb-tl-place web-post-tl))
                         (hull
                          (thumb-bl-place web-post-tr)
                          (thumb-bl-place (translate (wall-locate1 -0.3 1) web-post-tr))
                          (thumb-bl-place (translate (wall-locate2 -0.3 1) web-post-tr))
                          (thumb-bl-place (translate (wall-locate3 -0.3 1) web-post-tr))
                          (thumb-tl-place web-post-tl))
                         ; Tiny little piece leading to the left
                         (wall-brace thumb-bl-place  0  1 web-post-tr thumb-bl-place  0  1 web-post-tl)))

; NOTE: Using -1.5 instead of -1 to make these a bit bigger to make room for the hotswaps
(def wall-multiplier (if trackball-enabled 1.5 1))
(def trackball-tweeners (union
                         (wall-brace thumb-mr-place  0 (- wall-multiplier) web-post-br thumb-br-place  0 -1 web-post-br)))
(def back-convex-thumb-wall-0 ; thumb tweeners
  (if trackball-enabled
    trackball-tweeners
    (union
     (wall-brace thumb-mr-place  0 (- wall-multiplier) web-post-bl thumb-br-place  0 -1 web-post-br))))
(def back-convex-thumb-wall-1 (wall-brace thumb-mr-place  0 (- wall-multiplier) web-post-br thumb-tr-place 0 (- wall-multiplier) thumb-post-br))
(def back-convex-thumb-wall-2 (if trackball-enabled
                                ; Back right thumb to the middle one
                                (triangle-hulls
                                 (thumb-mr-place web-post-br)
                                 (thumb-mr-place web-post-bl)
                                 (thumb-br-place web-post-br))
                                (union
                                 (wall-brace thumb-mr-place  0 (- wall-multiplier) web-post-br thumb-mr-place  0 (- wall-multiplier) web-post-bl))))
(def thumb-walls  ; thumb walls
  (union
   (wall-brace thumb-bl-place -1  0 web-post-bl thumb-br-place -1  0 web-post-tl)
   (wall-brace thumb-br-place  0 -1 web-post-br thumb-br-place  0 -1 web-post-bl)
   (wall-brace thumb-br-place -1  0 web-post-tl thumb-br-place -1  0 web-post-bl)
   (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place -1  0 web-post-bl)))

(def thumb-corners ; thumb corners
  (union
   (wall-brace thumb-br-place -1  0 web-post-bl thumb-br-place  0 -1 web-post-bl)
   (if trackball-enabled nil (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place  0  1 web-post-tl))
   ))

(def pro-micro-wall (union
                     (for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
                     (for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
                     ))
(def back-pinky-wall (for [x (range 4 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl x       cornerrow 0 -1 web-post-br)))
(def non-thumb-walls (union
                      ; left wall
                      (for [y (range 0 lastrow)] (union (wall-brace (partial left-key-place y 1)       -1 0 web-post (partial left-key-place y -1) -1 0 web-post)
                                                        (hull (key-place 0 y web-post-tl)
                                                              (key-place 0 y web-post-bl)
                                                              (left-key-place y  1 web-post)
                                                              (left-key-place y -1 web-post))))
                      (for [y (range 1 lastrow)] (union (wall-brace (partial left-key-place (dec y) -1) -1 0 web-post (partial left-key-place y  1) -1 0 web-post)
                                                        (hull (key-place 0 y       web-post-tl)
                                                              (key-place 0 (dec y) web-post-bl)
                                                              (left-key-place y        1 web-post)
                                                              (left-key-place (dec y) -1 web-post))))
                      (wall-brace (partial key-place 0 0) 0 1 web-post-tl (partial left-key-place 0 1) 0 1 web-post)
                      (wall-brace (partial left-key-place 0 1) 0 1 web-post (partial left-key-place 0 1) -1 0 web-post)
                      ; front wall
                      (key-wall-brace 3 lastrow   0 -1 web-post-bl 3 lastrow 0.5 -1 web-post-br)
                      (key-wall-brace 3 lastrow 0.5 -1 web-post-br 4 cornerrow 0.5 -1 web-post-bl)

                      ;                            (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl (dec x) cornerrow 0 -1 web-post-br))
                      ; Right before the start of the thumb
                      (wall-brace thumb-tr-place  0 -1 thumb-post-br (partial key-place 3 lastrow)  0 -1 web-post-bl)))
(def case-walls
  (union
   right-wall
   back-pinky-wall
   pro-micro-wall
   non-thumb-walls
   back-convex-thumb-wall-0
   back-convex-thumb-wall-1
   back-convex-thumb-wall-2
   thumb-walls
   thumb-corners
   (if trackball-enabled nil thumb-to-left-wall)
   back-convex-thumb-wall-0
   ))