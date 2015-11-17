#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget color-picker (QDialog input)
  ((color :initarg :color :accessor value))
  (:default-initargs :color (c 0 0 0)))

(defvar *color-picker-input-done* T)

(define-subwidget (color-picker color-triangle) (make-instance 'color-triangle :color color))
(define-subwidget (color-picker rgb-color-slider) (make-instance 'rgb-color-slider :color color))
(define-subwidget (color-picker hsv-color-slider) (make-instance 'hsv-color-slider :color color))
(define-subwidget (color-picker color-history) (make-instance 'color-history :color color :color-count 10))

(define-subwidget (color-picker ok-button) (q+:make-qpushbutton "&Ok")
  (connect! ok-button (clicked) color-picker (accept)))

(define-subwidget (color-picker cancel-button) (q+:make-qpushbutton "&Cancel")
  (connect! cancel-button (clicked) color-picker (reject)))

(define-subwidget (color-picker layout) (q+:make-qvboxlayout color-picker)
  (setf (q+:fixed-size color-picker) (values 590 350))
  (let ((colors (q+:make-qhboxlayout)))
    (q+:add-widget colors color-triangle)
    (let ((sliders (q+:make-qvboxlayout)))
      (q+:add-widget sliders rgb-color-slider)
      (q+:add-widget sliders hsv-color-slider)
      (q+:add-stretch sliders 1)
      (q+:add-layout colors sliders))
    (q+:add-layout layout colors))
  (q+:add-widget layout color-history)
  (let ((buttons (q+:make-qhboxlayout)))
    (q+:add-stretch buttons 1)
    (q+:add-widget buttons ok-button)
    (q+:add-widget buttons cancel-button)
    (q+:add-stretch buttons 1)
    (q+:add-layout layout buttons)))

(define-slot (color-picker input-updated-triangle) ()
  (declare (connected color-triangle (input-updated)))
  (let ((*color-picker-input-done* NIL))
    (setf (value color-picker) (value color-triangle))))

(define-slot (color-picker input-updated-rgb) ()
  (declare (connected rgb-color-slider (input-updated)))
  (let ((*color-picker-input-done* NIL))
    (setf (value color-picker) (value rgb-color-slider))))

(define-slot (color-picker input-updated-hsv) ()
  (declare (connected hsv-color-slider (input-updated)))
  (let ((*color-picker-input-done* NIL))
    (setf (value color-picker) (value hsv-color-slider))))

(define-slot (color-picker input-updated-history) ()
  (declare (connected color-history (input-updated)))
  (setf (value color-picker) (value color-history)))

(define-slot (color-picker input-done) ()
  (declare (connected color-triangle (input-done)))
  (declare (connected rgb-color-slider (input-done)))
  (declare (connected hsv-color-slider (input-done)))
  (declare (connected color-history (input-done)))
  (setf (value color-picker) (value color-picker)))

(defmethod (setf value) :after (value (color-picker color-picker))
  (with-slots-bound (color-picker color-picker)
    (setf (value color-triangle) value)
    (setf (value rgb-color-slider) value)
    (setf (value hsv-color-slider) value)
    (when *color-picker-input-done*
      (setf (value color-history) value))))
