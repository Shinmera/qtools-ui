#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget rgb-color-slider (QWidget color-storing-input)
  ())

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

(define-slot (rgb-color-slider done) ()
  (declare (connected r (input-done)))
  (declare (connected g (input-done)))
  (declare (connected b (input-done)))
  (signal! rgb-color-slider (input-done)))

(define-slot (rgb-color-slider input-updated) ()
  (declare (connected r (input-updated)))
  (declare (connected g (input-updated)))
  (declare (connected b (input-updated)))
  (setf (value rgb-color-slider) (q+:make-qcolor (round (value r)) (round (value g)) (round (value b)))))

(defmethod (setf value) :after (value (rgb-color-slider rgb-color-slider))
  (with-slots-bound (rgb-color-slider rgb-color-slider)
    (let ((value (direct-value rgb-color-slider)))
      (setf (value r) (q+:red value))
      (setf (value g) (q+:green value))
      (setf (value b) (q+:blue value)))))

(define-widget hsv-color-slider (QWidget color-storing-input)
  ())

(define-initializer (hsv-color-slider hsv-color-slider)
  (setf (value hsv-color-slider) (value hsv-color-slider)))

(define-subwidget (hsv-color-slider h) (make-instance 'slider :minimum 0 :maximum 359 :stepping 1)
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

(define-slot (hsv-color-slider done) ()
  (declare (connected h (input-done)))
  (declare (connected s (input-done)))
  (declare (connected v (input-done)))
  (signal! hsv-color-slider (input-done)))

(define-slot (hsv-color-slider input-updated) ()
  (declare (connected h (input-updated)))
  (declare (connected s (input-updated)))
  (declare (connected v (input-updated)))
  (setf (value hsv-color-slider) (q+:qcolor-from-hsv (round (value h)) (round (value s)) (round (value v)))))

(define-slot (rgb-color-slider done) ()
  (declare (connected r (input-done)))
  (declare (connected g (input-done)))
  (declare (connected b (input-done)))
  (signal! rgb-color-slider (input-done)))

(defmethod (setf value) :after (value (hsv-color-slider hsv-color-slider))
  (with-slots-bound (hsv-color-slider hsv-color-slider)
    (let* ((value (direct-value hsv-color-slider))
           (hue (if (< (q+:hsv-hue value) 0) (round (value h)) (q+:hsv-hue value))))
      (setf (value h) hue)
      (setf (value s) (q+:saturation value))
      (setf (value v) (q+:value value))
      (set-gradient-points (q+:gradient (q+:brush (q+:palette (slot-value s 'double-slider)) (q+:qpalette.background)))
                           (q+:qcolor-from-hsv hue 0 (q+:value value))
                           (q+:qcolor-from-hsv hue 255 (q+:value value)))
      (set-gradient-points (q+:gradient (q+:brush (q+:palette (slot-value v 'double-slider)) (q+:qpalette.background)))
                           (q+:qcolor-from-hsv hue (q+:saturation value) 0)
                           (q+:qcolor-from-hsv hue (q+:saturation value) 255)))))

(define-override (hsv-color-slider update) ()
  (q+:update h)
  (q+:update s)
  (q+:update v)
  (stop-overriding))
