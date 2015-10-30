#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget listing (QWidget sorted-item-container selectable-layout)
  ((minimum-row-height :initarg :minimum-row-height :accessor minimum-row-height)
   (fixed-row-height :initarg :fixed-row-height :accessor fixed-row-height)
   (draggable :initarg :draggable :accessor draggable))
  (:default-initargs
    :minimum-row-height 20
    :fixed-row-height NIL
    :draggable T))

(defmethod update ((listing listing))
  (loop for y = 0 then (+ y height)
        for widget in (widgets listing)
        for hint = (q+:minimum-size-hint widget)
        for height = (or (fixed-row-height listing)
                         (max (minimum-row-height listing) (q+:minimum-height widget) (q+:height hint)))
        do (setf (q+:geometry widget) (values 0 y (q+:width listing) height))))

(defmethod (setf sorting) :after (sorting (listing listing))
  (when sorting (setf (draggable listing) NIL)))

(defmethod coerce-item (item (listing listing))
  (make-instance 'listing-item :item item :container listing))

(defmethod widget-acceptable-p ((widget qobject) (listing listing))
  NIL)

(define-widget listing-item (QWidget cell)
  ())

(defmethod drag :around ((listing-item listing-item) px py nx ny)
  (when (draggable (container listing-item))
    (call-next-method)))

(defmethod widget-acceptable-p ((listing-item listing-item) (listing listing))
  T)
