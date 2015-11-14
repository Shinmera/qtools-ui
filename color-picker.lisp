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
(define-subwidget (color-picker color-history) (make-instance 'color-history :color color))

(define-subwidget (color-picker ok-button) (q+:make-qpushbutton "&Ok")
  (connect! ok-button (clicked) color-picker (accept)))

(define-subwidget (color-picker cancel-button) (q+:make-qpushbutton "&Cancel")
  (connect! cancel-button (clicked) color-picker (reject)))

(define-subwidget (color-picker layout) (q+:make-qgridlayout color-picker)
  (q+:add-widget layout color-triangle 0 0 3 1)
  (q+:add-widget layout rgb-color-slider 0 1 1 1)
  (q+:add-widget layout hsv-color-slider 1 1 1 1)
  (q+:add-widget layout color-history 2 1 1 1)
  (let ((buttons (q+:make-qhboxlayout)))
    (q+:add-stretch buttons 1)
    (q+:add-widget buttons ok-button)
    (q+:add-widget buttons cancel-button)
    (q+:add-stretch buttons 1)
    (q+:add-layout layout buttons 3 0 1 2)))

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
