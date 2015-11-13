#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget color-triangle (QGLWidget repaintable input)
  ((color :initarg :color :finalized T :accessor value)
   (gradient :initform (make-circle-rainbow-gradient) :finalized T)
   (pressed :initform NIL))
  (:default-initargs
    :color (q+:make-qcolor 0 255 0)))

(define-initializer (color-triangle setup)
  (setf (value color-triangle) (value color-triangle))
  (setf (q+:minimum-width color-triangle) 100)
  (setf (q+:minimum-height color-triangle) 100)
  (setf (q+:horizontal-policy (q+:size-policy color-triangle))
        (q+:qsizepolicy.expanding)))

(defmethod (setf value) (value (color-triangle color-triangle))
  (setf (slot-value color-triangle 'color) (coerce-color value))
  (repaint color-triangle))

(define-override (color-triangle paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing* ((painter (q+:make-qpainter color-triangle))
                     (center (q+:make-qpointf (/ (q+:width color-triangle) 2)
                                              (/ (q+:height color-triangle) 2)))
                     (size (ctriangle-size color-triangle))
                     (width 10))
    (setf (q+:render-hint painter) (q+:qpainter.antialiasing))
    (setf (q+:render-hint painter) (q+:qpainter.high-quality-antialiasing))
    (with-translation (painter center)
      (ctriangle-draw-wheel painter gradient size width)
      (q+:rotate painter (- (q+:hue color)))
      (ctriangle-draw-triangle painter color size)
      (ctriangle-draw-ticks painter color size width))))

(define-override (color-triangle mouse-press-event) (ev)
  (when (= (enum-value (q+:button ev)) (q+:qt.left-button))
    (let* ((size (ctriangle-size color-triangle))
           (width 10)
           (x (- (q+:x ev) (/ (q+:width color-triangle) 2)))
           (y (- (q+:y ev) (/ (q+:height color-triangle) 2)))
           (r (sqrt (+ (expt x 2) (expt y 2)))))
      (cond
        ((< size r (+ size width))
         (setf pressed :wheel))
        ((< r size)
         (setf pressed :picker)))
      (ctriangle-mouse-move color-triangle ev))))

(define-override (color-triangle mouse-release-event) (ev)
  (when (= (enum-value (q+:button ev)) (q+:qt.left-button))
    (ctriangle-mouse-move color-triangle ev)
    (setf pressed NIL)))

(define-override (color-triangle mouse-move-event ctriangle-mouse-move) (ev)
  (let* ((size (ctriangle-size color-triangle))
         (width 10)
         (x (- (q+:x ev) (/ (q+:width color-triangle) 2)))
         (y (- (q+:y ev) (/ (q+:height color-triangle) 2))))
    (case pressed
      (:wheel
       (let ((p (round (+ 360 (- (/ (* (atan y x) 180) PI))))))
         (setf (q+:hsv color) (values p (q+:saturation color) (q+:value color))))
       (setf (value color-triangle) (value color-triangle)))
      (:picker
       (multiple-value-bind (s v) (xy-to-sv x y (q+:hsv-hue color) (- size width))
         (setf (q+:hsv color) (values (q+:hsv-hue color) s v)))
       (setf (value color-triangle) (value color-triangle))))))

(define-override (color-triangle size-hint) ()
  (q+:make-qsize 250 250))

(defun make-circle-rainbow-gradient ()
  (let ((gradient (q+:make-qconicalgradient 0 0 0)))
    (setf (q+:color-at gradient 0/6) (c 255 0 0))
    (setf (q+:color-at gradient 1/6) (c 255 255 0))
    (setf (q+:color-at gradient 2/6) (c 0 255 0))
    (setf (q+:color-at gradient 3/6) (c 0 255 255))
    (setf (q+:color-at gradient 4/6) (c 0 0 255))
    (setf (q+:color-at gradient 5/6) (c 255 0 255))
    (setf (q+:color-at gradient 6/6) (c 255 0 0))
    gradient))

(defun ctriangle-draw-wheel (painter gradient size width)
  (with-finalizing ((gradient-brush (q+:make-qbrush gradient)))
    (setf (q+:brush (q+:pen painter)) gradient-brush)
    (setf (q+:width (q+:pen painter)) width)
    (q+:draw-ellipse painter (q+:make-qpointf 0 0) (+ size (/ width 2)) (+ size (/ width 2)))))

(defun ctriangle-draw-triangle (painter color size)
  (with-finalizing ((full-color (q+:qcolor-from-hsv (q+:hsv-hue color) 255 255))
                    (side (* size (cos (/ (* PI 30) 180)))))
    (q+:begin-native-painting painter)
    (gl:enable :multisample)
    (gl:enable :line-smooth)
    (gl:with-primitives :triangles
      (gl:color (/ (q+:red full-color) 255)
                (/ (q+:green full-color) 255)
                (/ (q+:blue full-color) 255))
      (gl:vertex size 0)
      (gl:color 1.0 1.0 1.0)
      (gl:vertex (- (/ size 2)) side)
      (gl:color 0.0 0.0 0.0)
      (gl:vertex (- (/ size 2)) (- side)))
    (q+:end-native-painting painter)))

(defun ctriangle-draw-ticks (painter color size width)
  (multiple-value-bind (x y) (sv-to-xy (q+:saturation color) (q+:value color) (- size width))
    (with-finalizing ((point (q+:make-qpointf x y))
                      (brush (q+:make-qbrush (c 255 255 255))))
      (setf (q+:brush (q+:pen painter)) brush)
      (setf (q+:width (q+:pen painter)) 5)
      (q+:draw-line painter (round size) 0 (round (+ size width)) 0)
      (setf (q+:width (q+:pen painter)) 2)      
      (q+:draw-ellipse painter point 5 5))))

(defun ctriangle-size (color-triangle)
  (- (/ (min (q+:width color-triangle)
             (q+:height color-triangle)) 2)
     20))

(defun xy-to-sv (x y hue length)
  (let* ((p (/ (* (+ 210 hue) PI) 180))
         ;; Triangle side length
         (g (* 2 length (/ (sqrt 3) 2)))
         ;; Triangle height
         (h (* g (/ (sqrt 3) 2)))
         ;; Reverse rotation
         (x2 (- (* x (cos p))
                (* y (sin p))))
         (y2 (+ (* x (sin p))
                (* y (cos p))))
         ;; Calculate triangle coordinates
         (v (- (/ 2 3) (/ y2 h)))
         (s (+ (/ x2 (* v g)) (/ 1 2)))
         ;; Scale and cap
         (v (round (* (max (min v 1) 0) 255)))
         (s (round (* (- 1 (max (min s 1) 0)) 255))))
    (values s v)))

(defun sv-to-xy (s v length)
  (let* ((p (/ (* 150 PI) 180))
         (v (/ v 255))
         (s (- 1 (/ s 255)))
         ;; Triangle side length
         (g (* 2 length (/ (sqrt 3) 2)))
         ;; Triangle height
         (h (* g (/ (sqrt 3) 2)))
         ;; Calculate Cartesian coordinates
         (x (* v g (- s (/ 1 2))))
         (y (* h (- (/ 2 3) v)))
         ;; Turn into hue rotated coordinates
         (x2 (- (* x (cos p)) (/ y 2)))
         (y2 (+ (/ x 2) (* y (cos p)))))
    (values x2 y2)))
