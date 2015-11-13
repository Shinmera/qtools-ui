#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget rgb-color-slider (QWidget input)
  ((color :initarg :color :accessor value))
  (:default-initargs
    :color (c 0 0 0)))

(defun set-gradient-points (gradient &rest colors)
  (let ((step (/ (1- (length colors)))))
    (loop for color in colors
          for pos from 0 by step
          do (setf (q+:color-at gradient pos) color)))
  gradient)

(defun make-horizontal-gradient (&rest colors)
  (let ((gradient (q+:make-qlineargradient 0 0 1 0)))
    (apply #'set-gradient-points gradient colors)
    (setf (q+:coordinate-mode gradient) (q+:qgradient.stretch-to-device-mode))
    gradient))

(define-subwidget (rgb-color-slider r) (make-instance 'slider :minimum 0 :maximum 255 :stepping 1)
  (setf (q+:brush (q+:palette (slot-value r 'double-slider)) (q+:qpalette.background))
        (q+:make-qbrush (make-horizontal-gradient (c 0 0 0) (c 255 0 0))))
  (setf (q+:auto-fill-background (slot-value r 'double-slider)) T))

(define-subwidget (rgb-color-slider g) (make-instance 'slider :minimum 0 :maximum 255 :stepping 1)
  (setf (q+:brush (q+:palette (slot-value g 'double-slider)) (q+:qpalette.background))
        (q+:make-qbrush (make-horizontal-gradient (c 0 0 0) (c 0 255 0))))
  (setf (q+:auto-fill-background (slot-value g 'double-slider)) T))

(define-subwidget (rgb-color-slider b) (make-instance 'slider :minimum 0 :maximum 255 :stepping 1)
  (setf (q+:brush (q+:palette (slot-value b 'double-slider)) (q+:qpalette.background))
        (q+:make-qbrush (make-horizontal-gradient (c 0 0 0) (c 0 0 255))))
  (setf (q+:auto-fill-background (slot-value b 'double-slider)) T))

(define-subwidget (rgb-color-slider layout) (q+:make-qvboxlayout rgb-color-slider)
  (q+:add-widget layout r)
  (q+:add-widget layout g)
  (q+:add-widget layout b)
  (q+:add-stretch layout 1))

(define-slot (rgb-color-slider value-changed) ((value double))
  (declare (connected r (value-changed double)))
  (declare (connected g (value-changed double)))
  (declare (connected b (value-changed double)))
  (setf (value rgb-color-slider) (q+:make-qcolor (round (value r)) (round (value g)) (round (value b)))))

(defmethod (setf value) (value (rgb-color-slider rgb-color-slider))
  (let ((value (coerce-color value)))
    (with-slots-bound (rgb-color-slider rgb-color-slider)
      (setf (slot-value rgb-color-slider 'color) value)
      (setf (value r) (q+:red value))
      (setf (value g) (q+:green value))
      (setf (value b) (q+:blue value)))))


(define-widget hsv-color-slider (QWidget input)
  ((color :initarg :color :accessor value))
  (:default-initargs
    :color (c 0 0 0)))

(define-initializer (hsv-color-slider hsv-color-slider)
  (setf (value hsv-color-slider) (value hsv-color-slider)))

(define-subwidget (hsv-color-slider h) (make-instance 'slider :minimum 0 :maximum 360 :stepping 1)
  (setf (q+:brush (q+:palette (slot-value h 'double-slider)) (q+:qpalette.background))
        (q+:make-qbrush (make-horizontal-gradient
                         (c 255 0 0) (c 255 255 0) (c 0 255 0)
                         (c 0 255 255) (c 0 0 255) (c 255 0 255)
                         (c 255 0 0))))
  (setf (q+:auto-fill-background (slot-value h 'double-slider)) T))

(define-subwidget (hsv-color-slider s) (make-instance 'slider :minimum 0 :maximum 255 :stepping 1)
  (setf (q+:brush (q+:palette (slot-value s 'double-slider)) (q+:qpalette.background))
        (q+:make-qbrush (make-horizontal-gradient (c 0 0 0) (c 0 0 0))))
  (setf (q+:auto-fill-background (slot-value s 'double-slider)) T))

(define-subwidget (hsv-color-slider v) (make-instance 'slider :minimum 0 :maximum 255 :stepping 1)
  (setf (q+:brush (q+:palette (slot-value v 'double-slider)) (q+:qpalette.background))
        (q+:make-qbrush (make-horizontal-gradient (c 0 0 0) (c 0 0 0))))
  (setf (q+:auto-fill-background (slot-value v 'double-slider)) T))

(define-subwidget (hsv-color-slider layout) (q+:make-qvboxlayout hsv-color-slider)
  (q+:add-widget layout h)
  (q+:add-widget layout s)
  (q+:add-widget layout v)
  (q+:add-stretch layout 1))

(define-slot (hsv-color-slider value-changed) ((value double))
  (declare (connected h (value-changed double)))
  (declare (connected s (value-changed double)))
  (declare (connected v (value-changed double)))
  (setf (value hsv-color-slider) (q+:qcolor-from-hsv (round (value h)) (round (value s)) (round (value v)))))

(defmethod (setf value) (value (hsv-color-slider hsv-color-slider))
  (let ((value (coerce-color value)))
    (with-slots-bound (hsv-color-slider hsv-color-slider)
      (setf (slot-value hsv-color-slider 'color) value)
      (setf (value h) (max 0 (q+:hue value)))
      (setf (value s) (q+:saturation value))
      (setf (value v) (q+:value value))
      (set-gradient-points (q+:gradient (q+:brush (q+:palette (slot-value s 'double-slider)) (q+:qpalette.background)))
                           (q+:qcolor-from-hsv (q+:hue value) 0 (q+:value value))
                           (q+:qcolor-from-hsv (q+:hue value) 255 (q+:value value)))
      (set-gradient-points (q+:gradient (q+:brush (q+:palette (slot-value v 'double-slider)) (q+:qpalette.background)))
                           (q+:qcolor-from-hsv (q+:hue value) (q+:saturation value) 0)
                           (q+:qcolor-from-hsv (q+:hue value) (q+:saturation value) 255))
      (q+:update h) (q+:update s) (q+:update v))))
