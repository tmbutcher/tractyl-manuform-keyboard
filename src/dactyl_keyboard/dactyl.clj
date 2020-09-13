(ns dactyl-keyboard.dactyl
    (:refer-clojure :exclude [use import])
    (:require [clojure.core.matrix :refer [array matrix mmul]]
      [scad-clj.scad :refer :all]
      [scad-clj.model :refer :all]
      [unicode-math.core :refer :all]))

(defn deg2rad [degrees]
      (* (/ degrees 180) pi))

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 4)
(def ncols 5)
(def trackball-enabled true)

(def α (/ π 8))                        ; curvature of the columns
(def β (/ π 26))                        ; curvature of the rows
(def centerrow (- nrows 2.5))             ; controls front-back tilt
(def centercol 2)                       ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (deg2rad 20))            ; or, change this for more precise tenting control
(def column-style
  (if (> nrows 5) :orthographic :standard))  ; options include :standard, :orthographic, and :fixed
; (def column-style :fixed)
(def pinky-15u false)

(defn column-offset [column] (cond
                               (= column 2) [0 2.82 -4.5]
                               (= column 3) [0 -1 -4]
                               (>= column 4) [0 -16 -5.50]            ; original [0 -5.8 5.64]
                               :else [0 -5 1.5]))

(def thumb-offsets [6 0 10])

(def keyboard-z-offset 23.5)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 2.5)                   ; extra space between the base of keys; original= 2
(def extra-height 1.0)                  ; original= 0.5

(def wall-z-offset -5)                 ; original=-15 length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 5)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 3)                  ; wall thickness parameter; originally 5

;; Settings for column-style == :fixed
;; The defaults roughly match Maltron settings
;;   http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

; If you use Cherry MX or Gateron switches, this can be turned on.
; If you use other switches such as Kailh, you should set this as false
(def create-side-nubs? false)

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.2) ;; Was 14.1, then 14.25
(def keyswitch-width 14.2)

(def sa-profile-key-height 12.7)

(def plate-thickness 3)
(def side-nub-thickness 4)
(def retention-tab-thickness 1.5)
(def retention-tab-hole-thickness (- plate-thickness retention-tab-thickness))
(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))

(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) 1.5 plate-thickness)
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                       (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                   0
                                   (/ plate-thickness 2)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 side-nub-thickness)
                                 (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                             0
                                             (/ side-nub-thickness 2)])))
                      (translate [0 0 (- plate-thickness side-nub-thickness)]))
        plate-half (union top-wall left-wall (if create-side-nubs? (with-fn 100 side-nub)))
        top-nub (->> (cube 5 5 retention-tab-hole-thickness)
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
         (->>
           top-nub-pair
           (rotate (/ π 2) [0 0 1])))))

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap {1 (let [bl2 (/ 18.5 2)
                     m (/ 17 2)
                     key-cap (cube 18.25 18.25 2)]
                    (->> key-cap
                         (translate [0 0 (+ 4 plate-thickness)])
                         (color [220/255 163/255 163/255 1])))
             2 (let [bl2 sa-length
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                    (->> key-cap
                         (translate [0 0 (+ 5 plate-thickness)])
                         (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 27.94 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                      (->> key-cap
                           (translate [0 0 (+ 5 plate-thickness)])
                           (color [240/255 223/255 175/255 1])))})

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 ncols))
(def rows (range 0 nrows))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))

(defn offset-for-column [col]
      (if (and (true? pinky-15u) (= col lastcol)) 5.5 0))
(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
      (let [column-angle (* β (- centercol column))
            placed-shape (->> shape
                              (translate-fn [(offset-for-column column) 0 (- row-radius)])
                              (rotate-x-fn  (* α (- centerrow row)))
                              (translate-fn [0 0 row-radius])
                              (translate-fn [0 0 (- column-radius)])
                              (rotate-y-fn  column-angle)
                              (translate-fn [0 0 column-radius])
                              (translate-fn (column-offset column)))
            column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
            placed-shape-ortho (->> shape
                                    (translate-fn [0 0 (- row-radius)])
                                    (rotate-x-fn  (* α (- centerrow row)))
                                    (translate-fn [0 0 row-radius])
                                    (rotate-y-fn  column-angle)
                                    (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                    (translate-fn (column-offset column)))
            placed-shape-fixed (->> shape
                                    (rotate-y-fn  (nth fixed-angles column))
                                    (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                    (translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
                                    (rotate-x-fn  (* α (- centerrow row)))
                                    (translate-fn [0 0 (+ row-radius (nth fixed-z column))])
                                    (rotate-y-fn  fixed-tenting)
                                    (translate-fn [0 (second (column-offset column)) 0]))]
           (->> (case column-style
                      :orthographic placed-shape-ortho
                      :fixed        placed-shape-fixed
                      placed-shape)
                (rotate-y-fn  tenting-angle)
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
         [0 (Math/sin angle)    (Math/cos angle)]]
        position))

(defn rotate-around-y [angle position]
      (mmul
        [[(Math/cos angle)     0 (Math/sin angle)]
         [0                    1 0]
         [(- (Math/sin angle)) 0 (Math/cos angle)]]
        position))

(defn key-position [column row position]
      (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))

(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
              (->> single-plate
                   (key-place column row)))))

(def caps
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
              (->> (sa-cap (if (and (true? pinky-15u) (= column lastcol)) 1.5 1))
                   (key-place column row)))))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 2)
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def big-boi-web-post (->> (cube post-size (+ post-size 30) 10)
                   (translate [0 0 (- (+ (/ 10 -2)
                                      plate-thickness) 1)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

; wide posts for 1.5u keys in the main cluster

(if (true? pinky-15u)
  (do (def wide-post-tr (translate [(- (/ mount-width 1.2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
      (def wide-post-tl (translate [(+ (/ mount-width -1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
      (def wide-post-bl (translate [(+ (/ mount-width -1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
      (def wide-post-br (translate [(- (/ mount-width 1.2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post)))
  (do (def wide-post-tr web-post-tr)
      (def wide-post-tl web-post-tl)
      (def wide-post-bl web-post-bl)
      (def wide-post-br web-post-br)))

(defn triangle-hulls [& shapes]
      (apply union
             (map (partial apply hull)
                  (partition 3 1 shapes))))

(def connectors
  (apply union
         (concat
           ;; Row connections
           (for [column (range 0 (dec ncols))
                 row (range 0 lastrow)]
                (triangle-hulls
                  (key-place (inc column) row web-post-tl)
                  (key-place column row web-post-tr)
                  (key-place (inc column) row web-post-bl)
                  (key-place column row web-post-br)))

           ;; Column connections
           (for [column columns
                 row (range 0 cornerrow)]
                (triangle-hulls
                  (key-place column row web-post-bl)
                  (key-place column row web-post-br)
                  (key-place column (inc row) web-post-tl)
                  (key-place column (inc row) web-post-tr)))

           ;; Diagonal connections
           (for [column (range 0 (dec ncols))
                 row (range 0 cornerrow)]
                (triangle-hulls
                  (key-place column row web-post-br)
                  (key-place column (inc row) web-post-tr)
                  (key-place (inc column) row web-post-bl)
                  (key-place (inc column) (inc row) web-post-tl))))))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(def thumborigin
  (map + (key-position 1 cornerrow [(+ (/ mount-width 2) 14) (+ (- (/ mount-height 3)) -1) 2])
       thumb-offsets))

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
(def trackball-middle-translate [-7 4 -1.5])
(def thumb-tip-offset [-32.5 -16 -5])
(def thumb-tip-origin (map + thumborigin thumb-tip-offset))
(def tl-thumb-loc (map + thumb-tip-offset (if trackball-enabled trackball-middle-translate [0 0 0])))
(defn thumb-tl-place [shape]
      (->> shape
           (rotate (deg2rad  -16) [1 0 0])
           (rotate (deg2rad -54) [0 1 0])
           (rotate (deg2rad  35) [0 0 1]) ; original 10
           (translate thumborigin)
           (translate tl-thumb-loc))) ; original 1.5u (translate [-32 -15 -2])))

(def mr-thumb-loc (map + [-25 -37.5 -2] (if trackball-enabled trackball-middle-translate [0 0 0])))
(defn thumb-mr-place [shape]
      (->> shape
           (rotate (deg2rad  -16) [1 0 0])
           (rotate (deg2rad -54) [0 1 0])
           (rotate (deg2rad  35) [0 0 1])
           (translate thumborigin)
           (translate mr-thumb-loc)))

(def br-thumb-loc (map + [-36 -44 -20] (if trackball-enabled [2 -12 2] [0 0 0])))
(defn thumb-br-place [shape]
      (->> shape
           (rotate (deg2rad   -22) [1 0 0])
           (rotate (deg2rad -55) [0 1 0])
           (rotate (deg2rad  37) [0 0 1])
           (translate thumborigin)
           (translate br-thumb-loc)))

(def bl-thumb-loc (map + [-44 -23 -24] (if trackball-enabled [2 -12 2] [0 0 0])))
(defn thumb-bl-place [shape]
      (->> shape
           (rotate (deg2rad   -22) [1 0 0])
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

(def larger-plate
  (let [plate-height (- (/ (- sa-double-length mount-height) 3) 0.5)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))]
       (union top-plate (mirror [0 1 0] top-plate))))

(def thumbcaps
  (union
    (thumb-1x-layout (sa-cap 1))
    (thumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1)))))

(def thumb
  (union
    (thumb-1x-layout single-plate)
    (thumb-15x-layout single-plate)
    ; (thumb-15x-layout larger-plate)
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

(defn buckle [& {:keys [triangle-length triangle-width buckle-width-adjust buckle-width buckle-thickness buckle-length buckle-end-length buckle-height include-middle]}]
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

(defn buckle-holes [& {:keys [buckle-thickness buckle-width buckle-width-adjust triangle-length triangle-width buckle-height]}]
  (let [hole-x-translate (- (/ (+ buckle-width buckle-width-adjust) 2) (- triangle-width buckle-thickness) 0.2)]
    (union
     (translate [hole-x-translate 0 0]
                (cube (+ triangle-width 0.5) 10 (+ buckle-height 0.5) :center false))
     (translate [(- hole-x-translate) 0 0]
                (mirror [1 0] (cube (+ triangle-width 0.5) 10 (+ buckle-height 0.5) :center false))))))

;;;;;;;;;;;;;
;; Hotswap ;;
;;;;;;;;;;;;;

(def pin-cutout-height 3)
(def pin-height 11)
(def fillin-thickness 1.3)
(defn pin-slight-rotation [pin] (translate [0 0.7 0] (rotate(deg2rad 10) [1 0 0] pin)))
(def socket-pin-square
  (rotate (deg2rad 90) [1 0 0] (translate [0 6.5 (/ pin-cutout-height 2)] (cube 3 2.5 pin-cutout-height))))
(def socket-pin  (union
                                              socket-pin-square
                  (rotate (deg2rad 90) [1 0 0] (translate [(/ 1.3 -2) 0 fillin-thickness] (cube 1.3 pin-height (- pin-cutout-height fillin-thickness) :center false)))))
;; Hotswap socket test
(def socket-distance 5.5)
(def socket-height 5.6)
(def socket-width (+ socket-distance 4))
(def hotswap-buckle-length 4.2)
(def hotswap-top-wire-length 2.8)
(def grip-length 1)
(defn pins-place [socket-pin]
  (union
   (translate [(- (/ socket-distance 2)) pin-cutout-height -8.1] socket-pin)
   (translate [(/ socket-distance 2) pin-cutout-height -8.1] socket-pin))
  )
(def hotswap-socket-pins (pins-place socket-pin))
(def socket-join-height (- socket-height 3))
(def hotswap-clamp
  (let[grip-width    2
       grip-height 2
       grip            (polygon [[0 0] [grip-width 0] [grip-width grip-length] [0 grip-length]])
       thickness 1
       width (+ socket-width 0.25) ; give some wiggle room
       length (+ hotswap-buckle-length 0.25) ; give some wiggle room
       grip-offset     (+ (/ width 2) thickness)
       flat-model (union
                                         (translate [(/ width 2) (- length)] (square thickness length :center false))
                                         (translate [(/ width -2) (- length)] (mirror [1 0] (square thickness length :center false)))
                                         (translate [0 (- (+ length (/ thickness 2)))] (square (+ width (* 2 thickness)) thickness)))
       flat-grip-model (union
                                              (translate [(- grip-offset) 0] grip)
                                              (translate [grip-offset 0] (mirror [1 0] grip)))]
                          (union
                           (extrude-linear { :height socket-height } flat-model)
                           (translate [0 0 (/ (- grip-height socket-height) 2)] (extrude-linear { :height grip-height } flat-grip-model))
                           ; Bottom part of the holder
                           (let [bottom-width (+ width thickness thickness)
                                 bottom-length (+ length thickness grip-length)]
                             (difference
                              (translate [0 (+ (/ bottom-length -2) grip-length) (- (/ socket-height -2) (/ thickness 2))] (cube bottom-width bottom-length thickness))
                              (translate [0 (- hotswap-buckle-length) 0] (pin-slight-rotation hotswap-socket-pins)))))))
(def insert-path (translate [0 (/ 1.3 2) 0] (cube 1.3 1.3 pin-height)))
(def insert-path-x-offset (/ socket-distance 2))
(def hotswap-socket (difference
                     (translate [0 (/ hotswap-buckle-length 2) 0] (cube socket-width hotswap-buckle-length socket-height))
                     (pin-slight-rotation hotswap-socket-pins)
;                     (translate [insert-path-x-offset 0.7 0] insert-path)
;                     (translate [(- insert-path-x-offset) 0.7 0] insert-path)
                     (translate [0 -1.5 0] (pins-place socket-pin-square))
                     ))


(def plate-mount-buckle-width (- keyswitch-width 5))
(defn position-socket-clamp [shape] (->>
                                     shape
                                     (translate [0 hotswap-buckle-length 0])
                                     (rotate (deg2rad -30) [0 0 1])
                                     (translate [-3 0.5 (/ socket-height 2)])))
(def rotated-socket-clamp
  (->>
   hotswap-clamp
   position-socket-clamp))

(def distance-from-socket 1.6)
(def clamp-buckle-y-offset (+ -1 (- distance-from-socket)))
(def plate-mount-buckle-height 2)
(def hotswap-clamp-key-mount
  (union
   rotated-socket-clamp
   (->>
    (buckle
     :include-middle      false
     :triangle-length     2.5
     :triangle-width      4
     :buckle-width-adjust 0
     :buckle-width        plate-mount-buckle-width
     :buckle-thickness    2
     :buckle-length       (+ socket-height plate-thickness)
     :buckle-end-length   0
     :buckle-height       plate-mount-buckle-height)
    (rotate (deg2rad 90) [1 0 0])
    (translate [0  clamp-buckle-y-offset (+ socket-height plate-thickness)]))
   ; Connect the left buckle to the socket
   (hull
    (translate [(- (/ plate-mount-buckle-width -2) -3.5) -1.2 (/ socket-join-height 2)]
               (rotate (deg2rad -30) [0 0 1] (cube 7 0.1 socket-join-height)))
    (translate [(- (/ plate-mount-buckle-width -2) 1) clamp-buckle-y-offset (/ socket-join-height 2)]
               (cube 2 plate-mount-buckle-height socket-join-height))
    )
   ; Connect the right buckle to the socket
   (hull
    (->> (cube 0.1 hotswap-buckle-length socket-join-height)
         (translate
          [(/ (+ socket-width 0.6) 2)
           (/ (+ hotswap-buckle-length 0.5) -2)
           (+ (/ socket-join-height -2))])
         position-socket-clamp)
    (translate [(- (/ plate-mount-buckle-width 2) 1) -1.5 (/ socket-join-height 2)]
               (cube 1 1 socket-join-height))
    (translate [(+ (/ plate-mount-buckle-width 2) 0.5) clamp-buckle-y-offset (/ socket-join-height 2)]
               (cube 1 plate-mount-buckle-height socket-join-height)))))
(def buckle-hole-y-translate (+ (/ keyswitch-height 2) plate-mount-buckle-height distance-from-socket))
(def buckle-holes-on-key (->>
                          (buckle-holes
                           :buckle-thickness 2
                           :buckle-width plate-mount-buckle-width
                           :buckle-width-adjust 0
                           :triangle-width 4
                           :triangle-length 2.5
                           :buckle-height plate-mount-buckle-height)
                          (rotate (deg2rad 90) [1 0 0])
                          (translate [0 buckle-hole-y-translate -5])))
(def single-plate-with-hotswap (difference
                                (translate [0 2 0] (cube (+ keyswitch-width 4) (+ keyswitch-height 7) 3))
                                (cube keyswitch-width keyswitch-height 3)
                                buckle-holes-on-key))

(defn hotswap-place [hotswap] (let [
                                     bottom-hotswap (rotate (deg2rad 180) [0 0 1] hotswap)
                                     ] (union
                                        ; Bottom mounts
                                        (apply union
                                               (for [column columns
                                                     row [0 1]
                                                     :when (or (.contains [2 3] column)
                                                               (not= row lastrow))]
                                                 (->> bottom-hotswap
                                                      (key-place column row))))
                                        (apply union
                                               (for [column columns
                                                     row [2 3]
                                                     :when (or (.contains [2 3] column)
                                                               (not= row lastrow))]
                                                 (->> hotswap
                                                      (key-place column row))))
                                        (thumb-mr-place (if trackball-enabled bottom-hotswap hotswap))
                                        (thumb-br-place hotswap)
                                        (if trackball-enabled nil (thumb-tl-place bottom-hotswap))
                                        (thumb-bl-place bottom-hotswap)
                                        (thumb-tr-place bottom-hotswap))))

(def hotswap-holes (hotswap-place buckle-holes-on-key))

(def unified-pin-hotswap-mount (translate
                                [0 (- buckle-hole-y-translate distance-from-socket plate-mount-buckle-height 0.25) (- (- (+ socket-height plate-thickness) 1))]
                                (rotate (deg2rad 180) [0 0 1]
                                        (union
                                         hotswap-clamp-key-mount
                                         (->>
                                          (union
                                           (pin-slight-rotation hotswap-socket-pins)
                                           hotswap-socket)
                                          (translate [0 (- hotswap-buckle-length) 0])
                                          position-socket-clamp)))))

(def hotswap-tester (hotswap-place unified-pin-hotswap-mount))
(def single-hotswap-clearance
  (->>
   (cube (+ socket-width 2) (+ hotswap-buckle-length 2) (+ socket-height 2))
   (translate [0 (/ (+ hotswap-buckle-length 1) -2) -0.5])
   (translate [0 (- hotswap-buckle-length) 0])
   position-socket-clamp
   (rotate (deg2rad 180) [0 0 1])
   (translate
                         [0 (- buckle-hole-y-translate distance-from-socket plate-mount-buckle-height 0.25) (- (- (+ socket-height plate-thickness) 1))]
                        )))
(def hotswap-clearance (hotswap-place single-hotswap-clearance))


(spit "things/hotswap.scad" (write-scad (union hotswap-clamp-key-mount (position-socket-clamp (translate [0 (- hotswap-buckle-length) 0] hotswap-socket)))))
(spit "things/hotswap-on-key-test.scad" (write-scad (union
                                                     unified-pin-hotswap-mount
                                                     single-plate-with-hotswap)))
(spit "things/hotswap-clamp.scad" (write-scad hotswap-clamp-key-mount))
(spit "things/hotswap-socket.scad" (write-scad hotswap-socket))
(spit "things/socket-on-key.scad" (write-scad single-plate-with-hotswap))

;;;;;;;;;;;;;;;
;; Trackball ;;
;;;;;;;;;;;;;;;

(def dowel-depth-in-shell 1.5)
(def bearing-protrude (- 3 dowel-depth-in-shell)) ; Radius of the baring minus how deep it's going into the shell
(def trackball-width 34)
(def trackball-width-plus-bearing (+ bearing-protrude trackball-width 1)) ; Add one just to give some wiggle
(def holder-thickness 4.2)
(def outer-width (+ (* 2 holder-thickness) trackball-width-plus-bearing))

(def axel-angle 15)
(def dowell-width 3)
(def dowel-top-change 0)
(def dowel-top-height 1.5)
(def dowell-height 6) ; Dowel height is actually 6mm. But attempting to get it to "snap" in place
(def dowell (union (cylinder (- (/ dowell-width 2) dowel-top-change) (+ dowell-height dowel-top-height) :fn 50) (cylinder (/ dowell-width 2) dowell-height :fn 50)))
(def bearing (cylinder (/ 8.5 2) 3)) ; Bearing is actually 6mm x 2.5mm, model it as 8.5mm x 3 to give it room to spin
(def dowell-bearing (rotate (deg2rad 90) [1 0 0] (union dowell bearing)))
(defn rotated_dowell [angle]
  (rotate (deg2rad angle) [0, 0, 1] (rotate (deg2rad axel-angle) [0, 1, 0] (
                                                                             translate [(+ (/ trackball-width-plus-bearing 2) dowel-depth-in-shell) 0 0] (union
                                                                                                                                                          ; Add a cube on the side of the dowell so there's an insertion point when we diff with the shell
                                                                                                                                                          (translate [(- (/ dowell-width 2)) 0 0] (cube (+ dowell-width 1) (- dowell-height dowel-top-change) dowell-width))
                                                                                                                                                          dowell-bearing
                                                                                                                                                          )
                                                                                       )))
  )

(def dowells (union
              (rotated_dowell 0)
              (rotated_dowell 120)
              (rotated_dowell 240))
  )
(def vertical-hold 0) ; Millimeters of verticle hold after the curviture of the sphere ends to help hold the ball in

(def cup (
           difference
           (union
            (sphere (/ outer-width 2)) ; Main cup sphere
            (translate [0, 0, (/ vertical-hold 2)] (cylinder (/ outer-width 2) vertical-hold)) ; add a little extra to hold ball in
            )
           (sphere (/ trackball-width-plus-bearing 2))
           (translate [0, 0, (+ (/ outer-width 2) vertical-hold)] (cylinder (/ outer-width 2) outer-width)) ; cut out the upper part of the main cup spher
           )
  )

; We know the ball will sit approx bearing-protrude over the sensor holder. Eliminate the bottom and make it square
; up to that point with trim
(def trim (- (+ holder-thickness bearing-protrude) 0.5))
(def bottom-trim-origin [0 0 (- (- (/ outer-width 2) (/ trim 2)))])
(def bottom-trim ; trim the bottom off of the cup to get a lower profile
  (translate bottom-trim-origin (cube outer-width outer-width trim))
  )

(def holder-negatives (union
                       dowells
                       bottom-trim
                       )
  )
(def cup-bottom
  (translate [0 0 (- (- (/ outer-width 2) (/ trim 2)))] (cube outer-width outer-width trim))
  )
(def test-holder
  (
    difference
    cup
    holder-negatives
    )
  )

(def test-ball (sphere (/ trackball-width 2)))

(def test-holder-with-ball (union
                            (translate [0 0 (- (/ holder-thickness 2))] cup)
                            test-ball
                            ))

(defn clearance [extrax extray extraz]
  (translate [0 0 (/ 30 2)]
             (cube (+ keyswitch-width extrax) (+ keyswitch-width extray) extraz)
             )
  )

(def thumb-key-clearance (union
                          (thumb-1x-layout (clearance 0 0 30))
                          (thumb-15x-layout (rotate (/ π 2) [0 0 1] (clearance 2.5 2.5 30)))))

(def key-clearance (union
                    (apply union
                          (for [column columns
                                row rows
                                :when (or (.contains [2 3] column)
                                          (not= row lastrow))]
                            (->> (clearance keyswitch-width keyswitch-width 30)
                                 (key-place column row))))
                    (->> single-hotswap-clearance
                         (key-place 0 2))
                    (->> single-hotswap-clearance
                         (key-place 1 2))))

(defn trackball-mount-rotate [thing] (rotate (deg2rad -12) [0 0 1]
                                             (rotate (deg2rad 34) [1 0 0]
                                                     (rotate (deg2rad -39) [0 1 0] thing))
                                             ))

(def sensor-length 28)
(def sensor-width 22)
(def sensor-holder-width (/ sensor-width 2))
(def sensor-height 7.5)
(def sensor-holder-arm (translate [0 -0.5 0]
                                  (union
                                   (translate [0 (- (/ 4 2) (/ 1 2)) 1] (cube sensor-holder-width 4 2))
                                   (translate [0 0 (- (/ sensor-height 2))] (cube sensor-holder-width 1 sensor-height))
                                   (translate [0 (- (/ 4 2) (/ 1 2)) (- (+ sensor-height (/ 1 2)))] (cube sensor-holder-width 4 1))
                                   )))
(def sensor-holder
  (translate (map + bottom-trim-origin [0 0 (/ trim 2)])
             (union
              (translate [0 (- (/ sensor-length 2)) 0] sensor-holder-arm)
              (->>
               sensor-holder-arm
               (mirror [0 1 0])
               (translate [0 (/ sensor-length 2) 0])
               )
              )
             )
  )

(defn sensor-hole-angle [shape] (
                                  ->> shape
                                      (rotate (deg2rad -55) [0 1 0])
                                      (rotate (deg2rad 40) [0 0 1])
                                      ))
(defn dowell-angle [shape] (
                             ->> shape
                                 (rotate (deg2rad (+ 90 35)) [0 0 1])
                                 (rotate (deg2rad -30) [0 1 0])
                                 (rotate (deg2rad 25) [1 0 0])
                                 ))

(def rotated-dowells
  (dowell-angle
   (translate [0 0 (- (/ holder-thickness 2))] dowells)
   ))

(def rotated-bottom-trim     (sensor-hole-angle bottom-trim))

; This makes sure we can actually insert the trackball by leaving a column a little wider than it's width
(def trackball-insertion-cyl (dowell-angle (translate [0 0 (- (/ trackball-width 2) (/ holder-thickness 2))]
                             (cylinder (+ (/ trackball-width 2) 1) (+ (/ outer-width 2) 10))))
  )

(def trackball-raise (+ bearing-protrude 0.5))
(defn filler-rotate [p] (
                         ->> p
                             (trackball-mount-rotate)
                             ;                       (rotate (deg2rad 0) [0 1 0])
                             (rotate (deg2rad 20) [0 0 1])
                             ;                         (rotate (deg2rad -40) [1 0 0])
                         ))
(def filler-half-circle (
                   ->>  (
                          difference
                          (sphere (/ trackball-width-plus-bearing 2))
                          (translate [0 0 (+ (/ outer-width 2) vertical-hold)] (cylinder (/ outer-width 2) outer-width)) ; cut out the upper part of the main cup spher
                          )
                        (translate [0 0 trackball-raise])
                       filler-rotate
                   )
                  )

(def trackball-mount
  (union
   (difference
    (union
     (trackball-mount-rotate cup)
     (filler-rotate cup)
      )
    ; subtract out room for the axels
    rotated-dowells
    ; Subtract out the bottom trim clearing a hole for the sensor
    rotated-bottom-trim
    )
   (sensor-hole-angle sensor-holder)
   )
  )

(def raised-trackball (translate [0 0 trackball-raise] (sphere (+ (/ trackball-width 2) 0.5))))
(def trackball-origin (map + thumb-tip-origin [-10.5 10 -5]))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(def case-filler-cup (difference (translate trackball-origin filler-half-circle)
                                 key-clearance
                                 thumb-key-clearance
                                 (translate trackball-origin rotated-dowells)
                                 ))

(defn bottom [height p]
      (->> (project p)
           (extrude-linear {:height height :twist 0 :convexity 0})
           (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
      (hull p (bottom 0.001 p)))

(def left-wall-x-offset 5) ; original 10
(def left-wall-z-offset  3) ; original 3

(defn left-key-position [row direction]
      (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]))

(defn left-key-place [row direction shape]
      (translate (left-key-position row direction) shape))

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

(def trackball-walls
  (union
   ; clunky bit on the top left thumb connection  (normal connectors don't work well)
   ; merging with hulls to the trackball mount
   (difference
    (union
     ; Back right thumb to the middle one
     (triangle-hulls
      (thumb-mr-place web-post-br)
      (thumb-mr-place web-post-bl)
      (thumb-br-place web-post-br))
     ; Thumb to rest of case
     (bottom-hull
      (bottom 25 (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) big-boi-web-post)))
      ;             (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
      (thumb-bl-place web-post-tr)
      (thumb-bl-place web-post-tl))
     ; Trackball mount to left outside of case
     (hull
      (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) big-boi-web-post))
      case-filler-cup)
     ; Gap between trackball mount and top key
     (hull
      (key-place 0 cornerrow web-post-bl)
      (key-place 0 cornerrow web-post-br)
      (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) big-boi-web-post)))
     ; Between the trackball and the outside of the case near the bottom, to ensure a nice seal
     (hull
      (bottom 25 (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) big-boi-web-post)))
      (translate trackball-origin (trackball-mount-rotate cup))))
    key-clearance
    thumb-key-clearance
    (translate trackball-origin rotated-bottom-trim)
    (translate trackball-origin rotated-dowells))))

(def trackball-tweeners (union
  (wall-brace thumb-mr-place  0 -1 web-post-br thumb-br-place  0 -1 web-post-br)
  (wall-brace thumb-bl-place -1  0 web-post-bl thumb-br-place -1  0 web-post-tl)
  (wall-brace thumb-tr-place  0 -1 thumb-post-br (partial key-place 3 lastrow)  0 -1 web-post-bl)))

(def case-walls
  (union
    right-wall
    ; back wall
    (for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
    (for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))

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
    (for [x (range 4 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl x       cornerrow 0 -1 web-post-br)) ; TODO fix extra wall
    (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl (dec x) cornerrow 0 -1 web-post-br))
    ; thumb walls
    (wall-brace thumb-mr-place  0 -1 web-post-br thumb-tr-place  0 -1 thumb-post-br)
   ;  TODO
    (if trackball-enabled nil (wall-brace thumb-mr-place  0 -1 web-post-br thumb-mr-place  0 -1 web-post-bl))
    (wall-brace thumb-br-place  0 -1 web-post-br thumb-br-place  0 -1 web-post-bl)
    (wall-brace thumb-bl-place  0  1 web-post-tr thumb-bl-place  0  1 web-post-tl)
    (wall-brace thumb-br-place -1  0 web-post-tl thumb-br-place -1  0 web-post-bl)
    (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place -1  0 web-post-bl)
    ; thumb corners
    (wall-brace thumb-br-place -1  0 web-post-bl thumb-br-place  0 -1 web-post-bl)
    (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place  0  1 web-post-tl)
    ; thumb tweeners
   (if trackball-enabled
     trackball-tweeners
     (union
      (wall-brace thumb-mr-place  0 -1 web-post-bl thumb-br-place  0 -1 web-post-br)
      (wall-brace thumb-bl-place -1  0 web-post-bl thumb-br-place -1  0 web-post-tl)
      (wall-brace thumb-tr-place  0 -1 thumb-post-br (partial key-place 3 lastrow)  0 -1 web-post-bl)))
   (if
     trackball-enabled
     trackball-walls
     (union
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
       )
     )
   ))

(def usb-holder-ref (key-position 0 0 (map - (wall-locate2  0  -1) [0 (/ mount-height 2) 0])))

(def usb-holder-position (map + [5 16 0] [(first usb-holder-ref) (second usb-holder-ref) 2]))
(def usb-holder-cube   (cube 18.5 37 2))
(def usb-holder-holder (translate (map + usb-holder-position [5 -12.3 0]) (difference (cube 21 39 4) (translate [0 0 1] usb-holder-cube))))

(def usb-jack (translate (map + usb-holder-position [5 10 4]) (cube 8.1 20 3.1)))

(def pro-micro-position (map + (key-position 0 1 (wall-locate3 -1 0)) [-6 2 -15]))
(def pro-micro-space-size [4 10 12]) ; z has no wall;
(def pro-micro-wall-thickness 2)
(def pro-micro-holder-size [(+ pro-micro-wall-thickness (first pro-micro-space-size)) (+ pro-micro-wall-thickness (second pro-micro-space-size)) (last pro-micro-space-size)])
(def pro-micro-space
  (->> (cube (first pro-micro-space-size) (second pro-micro-space-size) (last pro-micro-space-size))
       (translate [(- (first pro-micro-position) (/ pro-micro-wall-thickness 2)) (- (second pro-micro-position) (/ pro-micro-wall-thickness 2)) (last pro-micro-position)])))
(def pro-micro-holder
  (difference
    (->> (cube (first pro-micro-holder-size) (second pro-micro-holder-size) (last pro-micro-holder-size))
         (translate [(first pro-micro-position) (second pro-micro-position) (last pro-micro-position)]))
    pro-micro-space))

(def trrs-holder-size [6.2 10 3]) ; trrs jack PJ-320A
(def trrs-holder-hole-size [6.2 10 6]) ; trrs jack PJ-320A
(def trrs-holder-position  (map + usb-holder-position [20.5 0 0]))
(def trrs-holder-thickness 2)
(def trrs-holder-thickness-2x (* 2 trrs-holder-thickness))
(def trrs-holder
  (union
    (->> (cube (+ (first trrs-holder-size) trrs-holder-thickness-2x) (+ trrs-holder-thickness (second trrs-holder-size)) (+ (last trrs-holder-size) trrs-holder-thickness))
         (translate [(first trrs-holder-position) (second trrs-holder-position) (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2)]))))
(def trrs-holder-hole
  (union

    ; circle trrs hole
    (->>
      (->> (binding [*fn* 30] (cylinder 3.3 20))) ; 5mm trrs jack
      (rotate (deg2rad  90) [1 0 0])
      (translate [(first trrs-holder-position) (+ (second trrs-holder-position) (/ (+ (second trrs-holder-size) trrs-holder-thickness) 2)) (+ 3 (/ (+ (last trrs-holder-size) trrs-holder-thickness) 0.9))])) ;1.5 padding

    ; rectangular trrs holder
    (->> (apply cube trrs-holder-hole-size) (translate [(first trrs-holder-position) (+ (/ trrs-holder-thickness -2) (second trrs-holder-position)) (+ (/ (last trrs-holder-hole-size) 2) trrs-holder-thickness)]))))

(defn screw-insert-shape [bottom-radius top-radius height]
      (union
        (->> (binding [*fn* 30]
                      (cylinder [bottom-radius top-radius] height)))
        (translate [0 0 (/ height 2)] (->> (binding [*fn* 30] (sphere top-radius))))))

(defn screw-insert [column row bottom-radius top-radius height offset]
      (let [shift-right   (= column lastcol)
            shift-left    (= column 0)
            shift-up      (and (not (or shift-right shift-left)) (= row 0))
            shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
            position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                                           (if shift-down  (key-position column row (map - (wall-locate2  0 -1) [0 (/ mount-height 2) 0]))
                                                           (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                                                                          (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))]
           (->> (screw-insert-shape bottom-radius top-radius height)
                (translate (map + offset [(first position) (second position) (/ height 2)])))))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
      (union (screw-insert 0 0         bottom-radius top-radius height [7.5 7 0])
             (screw-insert 0 lastrow   bottom-radius top-radius height [0 15 0])
             ;  (screw-insert lastcol lastrow  bottom-radius top-radius height [-5 13 0])
             ;  (screw-insert lastcol 0         bottom-radius top-radius height [-3 6 0])
             (screw-insert lastcol lastrow  bottom-radius top-radius height [-3.5 17 0])
             (screw-insert lastcol 0         bottom-radius top-radius height [-1 2 0])
             (screw-insert 1 lastrow         bottom-radius top-radius height [-2.5 -16 0])))

; Hole Depth Y: 4.4
(def screw-insert-height 4)

; Hole Diameter C: 4.1-4.4
(def screw-insert-bottom-radius (/ 4.0 2))
(def screw-insert-top-radius (/ 3.9 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))

(spit "things/screw-test.scad"
      (write-scad
       (difference
        (screw-insert 0 0 (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1.5) [0 0 0])
        (screw-insert 0 0 screw-insert-bottom-radius screw-insert-top-radius screw-insert-height [0 0 0])
         )
       ))

; Wall Thickness W:\t1.65
(def screw-insert-case-radius 1.5)
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1.5)))
(def screw-insert-screw-holes  (screw-insert-all-shapes screw-insert-case-radius screw-insert-case-radius 350))

(def pinky-connectors
  (apply union
         (concat
           ;; Row connections
           (for [row (range 0 lastrow)]
                (triangle-hulls
                  (key-place lastcol row web-post-tr)
                  (key-place lastcol row wide-post-tr)
                  (key-place lastcol row web-post-br)
                  (key-place lastcol row wide-post-br)))

           ;; Column connections
           (for [row (range 0 cornerrow)]
                (triangle-hulls
                  (key-place lastcol row web-post-br)
                  (key-place lastcol row wide-post-br)
                  (key-place lastcol (inc row) web-post-tr)
                  (key-place lastcol (inc row) wide-post-tr)))
           ;;
           )))

(def pinky-walls
  (union
    (key-wall-brace lastcol cornerrow 0 -1 web-post-br lastcol cornerrow 0 -1 wide-post-br)
    (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 0 1 wide-post-tr)))


(def plate2d (cut
              (translate [0 0 -0.1]
                         (union case-walls
                                pinky-walls
                                ))))

(def bigplate
  (hull
    plate2d
   (circle 1))
  )

(def cutout-plate
  (color [220/255 163/255 163/255 1]
         (difference
          bigplate
          plate2d
          )
         )

  )

(def bigattempt
  (union
    plate2d
   (color [220/255 163/255 163/255 1] (resize [121 141 126] cutout-plate))
   ))
;(def plate-attempt bigattempt)

(def plate-attempt (difference
                    (extrude-linear {:height 2}
                                    bigattempt
                                    )
                    (translate [0 0 -10] screw-insert-screw-holes)
                    ))


(spit "things/test.scad"
      (write-scad
        (difference trrs-holder trrs-holder-hole)))

(def trackball-mount-translated-to-model (difference
   (translate trackball-origin
              trackball-mount
                                              )
   ; Subtract out clearance so there's nothing above the keys
   key-clearance
   thumb-key-clearance
   ))

(def hand-on-test
  (translate [-5 -60 92]
             (rotate (deg2rad -27) [1 0 0]
                     (rotate (deg2rad 12) [0 0 1]
                             (rotate (+ tenting-angle (deg2rad 5)) [0 1 0]
                                     (rotate
                                      (deg2rad -90) [1 0 0]
                                      (mirror [0 1 0] hand)
                                      )
                                     )
                             )
                     )
             ))

(def tent-ball-rad 7)
(def tent-stand-rad 5)
(def crank-rad 1.5)
(def crank-len 20)
(def tent-stand-thread-height 25)
(def tent-stand-thread-lead 1.25)
(def tent-thread (call-module "thread" tent-stand-rad tent-stand-thread-height tent-stand-thread-lead))
(def tent-stand (union
                  tent-thread
                 (translate [0 0 (- tent-stand-rad)] (sphere tent-ball-rad))
                  ))


(def tent-foot-width 25)
(def tent-foot-height 30)
(def tent-foot-thickness 2)
(def tent-ball-holder-thickness 4)
(def hook-angle 40)

; Some convoluted logic to create a little hook to hold the ball in
(def ball-hook
  (rotate (deg2rad 90) [1 0 0]
          (union
           (translate [0 (/ tent-ball-rad 2) 0]
                      (rotate (deg2rad 90) [1 0 0] (cube tent-ball-holder-thickness tent-ball-holder-thickness tent-ball-rad)))
           (translate [(- (+ tent-ball-rad (/ tent-ball-holder-thickness 2))) tent-ball-rad 0]
                      (extrude-rotate {:angle hook-angle :convexity 10} (translate [(+ tent-ball-rad (/ tent-ball-holder-thickness 2)) 0]
                                                                                   (square tent-ball-holder-thickness tent-ball-holder-thickness)))
                      )
           )
          ))

(defn rotated-ball-hook [angle]
  (rotate (deg2rad angle) [0 0 1] (translate [(+ tent-ball-rad (/ tent-ball-holder-thickness 2)) 0 (/ tent-foot-thickness 2)] ball-hook))
  )

(def tent-foot (union
                (cube tent-foot-width tent-foot-height tent-foot-thickness)
                (rotated-ball-hook 0)
                (rotated-ball-hook 90)
                (rotated-ball-hook 180)
                (rotated-ball-hook 270)
                 ))

(def thumb-tent-origin [-8 -70 -1])
(def index-tent-origin [-48 17 -1])

(def tent-nut-height 6)
(def tent-thread
  (rotate (deg2rad 180) [0 1 0]
          (call-module "thread" (+ tent-stand-rad 0.5) tent-nut-height tent-stand-thread-lead)
          )
  )
(def tent-nut (difference
               (translate [0 0 (/ tent-nut-height 2)] (cylinder (+ tent-stand-rad 1.5) tent-nut-height))
               tent-thread
                ))

;;;;;;;;;;;;;;;
;; Palm Rest ;;
;;;;;;;;;;;;;;;
(def palm-length 120)
(def palm-width 90)
(def palm-cutoff 32)
(def palm-support (translate [0 0 (- palm-cutoff)] (difference
                   (resize [palm-width palm-length 80] (sphere 240))
                   (translate [0 0 (- (- 100 palm-cutoff))] (cube 400 400 200))
                    )))

(defn palm-rest-hole-rotate [h] (rotate (deg2rad -8) [0 0 1] h))
(def palm-hole-origin (map + (key-position 3 (+ cornerrow 1) (wall-locate3 0 -1)) [-1.5 -8 -11]) )

(def triangle-length 7)
(def triangle-width 5)
; Make the buckle holes 2mm longer because the holes to the case aren't perfectly straight, which causes some problems.
(def buckle-width-adjust 0)
(def buckle-width 12)
(def buckle-thickness 3)
(def buckle-length 3.7)
(def buckle-end-length 5)
(def buckle-height 4)
(def buckle-end-width (- buckle-width (* 2 buckle-thickness)))
(def palm-buckle (buckle
                  :include-middle true
                  :triangle-length triangle-length
                  :triangle-width triangle-width
                  :buckle-width-adjust buckle-width-adjust
                  :buckle-width buckle-width
                  :buckle-thickness buckle-thickness
                  :buckle-length buckle-length
                  :buckle-end-length buckle-end-length
                  :buckle-height buckle-height
                   ))
(def palm-buckle-holes (buckle-holes
                        :buckle-thickness buckle-thickness
                        :buckle-width buckle-width
                        :buckle-width-adjust buckle-width-adjust
                        :triangle-width triangle-width
                        :triangle-length triangle-length
                        :buckle-height buckle-height
                         ))
(def palm-rod-top-length 44)
(def palm-rod-bottom-length 10.5)
(def palm-rod-length (+ palm-rod-top-length palm-rod-bottom-length))
(def palm-attach-rod (union
                      (translate [0 (- (+ (/ 5 2) buckle-length)) (- (/ palm-rod-length 2) palm-rod-bottom-length)]
                                 (cube buckle-end-width 5 palm-rod-length))
                      palm-buckle))

(def positioned-palm-support (->> palm-support
                                  (rotate (deg2rad 20) [0 0 1])
                                  (rotate (deg2rad 5) [1 0 0])
                                  (rotate (+ tenting-angle (deg2rad 14)) [0 1 0])
                                  (translate [2 -30 (- palm-rod-top-length 22)]
                                             )))
(def palm-rest (union
                positioned-palm-support
                ; Subtract out the part of the rod that's sticking up
                (difference
                 palm-attach-rod
                 (hull
                  positioned-palm-support
                  (translate [0 (- (+ (/ 3 2) buckle-length)) (+ palm-rod-top-length 5)]
                             (cube buckle-width 3 0.2))))))

(spit "things/palm-rest.scad" (
                           write-scad palm-rest
                           ))

(spit "things/palm-attach-test.scad" (write-scad
                                       palm-attach-rod
                                       ))

(def model-right-initial
  (difference
   (union
    key-holes
    pinky-connectors
    pinky-walls
    connectors
    thumb
    thumb-connectors
    (difference (union
                 case-walls
                 screw-insert-outers
                 usb-holder-holder
                 trrs-holder)
                ; Leave room to insert the ball
                (if trackball-enabled (translate trackball-origin trackball-insertion-cyl) nil)
                usb-jack
                trrs-holder-hole
                screw-insert-holes
                (translate palm-hole-origin (palm-rest-hole-rotate palm-buckle-holes))))
   hotswap-holes
   hotswap-clearance
   (translate [0 0 -20] (cube 350 350 40))))


(def trackball-subtract (union
                         ; Subtract out the actual trackball
                         (translate trackball-origin (dowell-angle raised-trackball))
                         ; Subtract out space for the cup, because sometimes things from the keyboard creep in
                         (translate trackball-origin (sphere (/ trackball-width-plus-bearing 2)))
                         ; Just... double check that we have the full dowell negative
                         (translate trackball-origin rotated-dowells)
                         hotswap-clearance))
(def model-right-with-trackball (difference
                                 (union
                                   model-right-initial
                                   trackball-mount-translated-to-model)
                                 trackball-subtract))

(spit "things/trackball-test.scad" (write-scad
                                    (difference
                                    (union
                                     trackball-mount-translated-to-model
                                     trackball-walls)
                                     trackball-subtract
                                     thumb-key-clearance
                                     (translate [0 0 -20] (cube 350 350 40)))))
(def model-right (if trackball-enabled model-right-with-trackball model-right-initial))

;(spit "things/palm-rest.scad" (write-scad palm-rest))

(spit "things/right-plate.scad"
      (write-scad
       (include "/Users/nprince/apps/dactyl-manuform-mini-keyboard/nutsnbolts/cyl_head_bolt.scad")
       (difference
        (union
         plate-attempt
         (translate thumb-tent-origin tent-nut)
         (translate index-tent-origin tent-nut)
         )
        (translate thumb-tent-origin tent-thread)
        (translate index-tent-origin tent-thread)
        )

       ))

(spit "things/tent-nut.scad" (write-scad
                              (include "/Users/nprince/apps/dactyl-manuform-mini-keyboard/nutsnbolts/cyl_head_bolt.scad")
                              tent-nut))

(spit "things/tent-foot.scad"
      (write-scad tent-foot)
      )
(spit "things/tent-stand.scad"
      (write-scad
       (include "/Users/nprince/apps/dactyl-manuform-mini-keyboard/nutsnbolts/cyl_head_bolt.scad")
       tent-stand
       )
      )

(spit "things/left.scad"
      (write-scad (mirror [-1 0 0] model-right)))
(spit "things/tent-all.scad" (write-scad
                              (include "/Users/nprince/apps/dactyl-manuform-mini-keyboard/nutsnbolts/cyl_head_bolt.scad")
                              (union
                                tent-foot
                               (translate [0 0 (+ 3 tent-ball-rad (/ tent-foot-thickness 2))] tent-stand)
                                )
                               ))

(spit "things/right-test.scad"
      (write-scad
       (difference
        (union
;         hand-on-test
         hotswap-tester
         model-right
;         (translate (map + palm-hole-origin [0 (+ buckle-length 3) (/ buckle-height 2)])
;                    (palm-rest-hole-rotate palm-rest))
         (if trackball-enabled (translate trackball-origin test-ball) nil)
         thumbcaps
         caps)

        (translate [0 0 -20] (cube 350 350 40)))))

(spit "things/right.scad" (write-scad (difference
                                        model-right
;                                       (translate (key-position 0 1 [-20 20 0]) (cube 49 70 200))
;                                       (translate (key-position 3 3 [10 10 0]) (cube 60 30 200))
;                                       (translate (key-position 2 2 [14 -4 0]) (cube 41 28 200))
;                                       (translate (key-position 4 0 [-10 24 0]) (cube 80 32 200))
;                                       (translate (key-position 4 3 [0 0 0]) (cube 80 40 200))
                                       )))

(defn -main [dum] 1)  ; dummy to make it easier to batch
