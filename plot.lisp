#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget plot (QWidget repaintable)
  ((max-y :initarg :max-y :accessor max-y)
   (min-y :initarg :min-y :accessor min-y)
   (step-x :initarg :step-x :accessor step-x)
   (grid :initarg :grid :accessor grid)
   (data :initarg :data :accessor data)
   (adjust :initarg :adjust :accessor adjust))
  (:default-initargs
    :max-y 100
    :min-y -100
    :step-x NIL
    :grid NIL
    :adjust T
    :data (make-array 0 :adjustable T :fill-pointer T)))

(defmethod (setf data) :after (value (plot plot))
  (repaint plot))

(defmethod add-point (y (plot plot))
  (vector-push-extend y (data plot))
  (when (adjust plot)
    (when (< y (min-y plot)) (setf (min-y plot) (- y 10)))
    (when (< (max-y plot) y) (setf (max-x plot) (+ y 10))))
  (repaint plot))

(defmethod add-points ((plot plot) &rest points)
  (let ((num (length points)))
    (dolist (y points)
      (vector-push-extend y (data plot) num)
      (when (adjust plot)
        (when (< (- y 10) (min-y plot)) (setf (min-y plot) (- y 10)))
        (when (< (max-y plot) (+ y 10)) (setf (max-y plot) (+ y 10))))))
  (repaint plot))

(define-override (plot paint-event) (ev)
  (flet ((realy (y)
           (- (q+:height plot) (- (* y (/ (q+:height plot) (- max-y min-y)))
                                  (* min-y (/ (q+:height plot) (- max-y min-y)))))))
    (with-finalizing ((painter (q+:make-qpainter plot))
                      (line (q+:make-qlinef))
                      (step (or step-x (/ (q+:width plot) (1- (length data)))))
                      (back-c (q+:make-qcolor 0 0 0))
                      (grid-c (q+:make-qcolor 100 100 100))
                      (axis-c (q+:make-qcolor 255 255 255))
                      (line-c (q+:make-qcolor 100 100 255))
                      (x-axis (q+:make-qlinef 0 (realy 0) (q+:width plot) (realy 0))))
      (setf (q+:render-hint painter) (q+:qpainter.antialiasing))
      (setf (q+:render-hint painter) (q+:qpainter.high-quality-antialiasing))
      (q+:fill-rect painter (q+:rect plot) back-c)
      (when grid
        (let ((grid (if (numberp grid) grid (/ (- max-y min-y) 10))))
          (setf (q+:color (q+:pen painter)) grid-c)
          (loop for x from 0 below (q+:width plot) by step
                do (setf (q+:line line) (values x 0 x (q+:height plot)))
                   (q+:draw-line painter line))
          (loop for y from min-y below max-y by grid
                do (setf (q+:line line) (values 0 (realy y) (q+:width plot) (realy y)))
                   (q+:draw-line painter line))))
      (setf (q+:color (q+:pen painter)) axis-c)
      (q+:draw-line painter x-axis)
      (setf (q+:color (q+:pen painter)) line-c)
      (loop for i from 1 below (length data)
            for el = (aref data i)
            for prevx = 0 then x
            for x from step by step
            for prevy = (realy (aref data 0)) then y
            for y = (realy el)
            do (setf (q+:line line) (values prevx prevy x y))
               (q+:draw-line painter line)))))
