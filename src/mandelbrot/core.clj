(ns mandelbrot.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [mandelbrot.complex :refer :all]))

(def zoom-speed 2.0)

(defn scale-px
  "Scale a pixel x/y to a complex point."
  [x y center zoom]
  (complex+ center (complex (/ x zoom) (/ y zoom))))

(defn calculate-point
  "Calculates the escape iteration of a point."
  ([max-depth c] (calculate-point max-depth c (complex 0.0) 0))
  ([max-depth c z i]
  (let [z-squared (complex* z z)
        zn (complex+ z-squared c)
        r (:real (complex* (complex-conj z) z))]
    (if (< 4 r)
      i
      (if (or (< max-depth i) (= z zn)) ; checks for a cycle from last iter
        0
        (recur max-depth c zn (+ 1 i)))))))

(defn calculate-pixels
  "Calculates the escape iteration of each pixe."
  [center zoom]
  (let [w (q/width)
        h (q/height)
        max-depth (* 10 (q/log zoom))]
    (apply vector (for [x (range w)]
     (apply vector (for [y (range h)]
       (calculate-point max-depth (scale-px
                                    (- x (/ w 2))
                                    (- y (/ h 2))
                                    center
                                    zoom))))))
    ))

(defn int-to-color
  "Transform an integer into an hsb color."
  [n]
  (let [i (mod n 256)]
    (if (= n 0)
      (list 0 0 0)
      (list i 255 255))))

(defn draw-points
  "Paint each pixel."
  ([pixels w h] (draw-points pixels w h 0 0))
  ([pixels w h i j] (if (not= j h)
                      (let [col (nth pixels i)
                            px (nth col j)
                            color (int-to-color px)]
                          (apply q/stroke color)
                          (q/point i j)
                          (recur pixels
                                 w
                                 h
                                (mod (+ i 1) w)
                                (if (= (+ i 1) w)
                                  (+ j 1)
                                  j))))))

(defn setup []
  (let [center (complex -1.478354895846 -0.007061329759)
        zoom 50.0]
    ; Set frame rate to 30 frames per second.
    (q/frame-rate 30)
    ; Set color mode to HSB (HSV) instead of default RGB.
    (q/color-mode :hsb)
    ; setup function returns initial state. It contains
    ; circle color and position.
    {:center center
     :zoom zoom
     :pixels (calculate-pixels center zoom)}))

(defn update-state [state]
  (let [center (:center state)
        zoom (* (:zoom state) zoom-speed)
        pixels (calculate-pixels center zoom)]
  {:center center :zoom zoom :pixels pixels}))

(defn draw-state [state]
  (let [w (q/width)
        h (q/height)]
    (q/background 0)
    (draw-points (:pixels state) w h)))

(q/defsketch mandelbrot
  :title "A day-glow pterodactyl!"
  :size [256 256]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
