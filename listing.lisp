#|
This file is a part of Qtools-UI
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defgeneric active-widget (listing))
(defgeneric active-item (listing))

(define-widget listing (QWidget container)
  ((active-widget :initarg :active-widget :accessor active-widget))
  (:default-initargs :active-widget NIL))

(defmethod update ((listing listing))
  (loop for y = 0 then (+ y height)
        for widget in (widgets listing)
        for hint = (q+:minimum-size-hint widget)
        for height = (max 0 (q+:minimum-height widget) (q+:height hint))
        do (setf (q+:geometry widget) (values 0 y (q+:width listing) height))))

(defmethod (setf active-widget) ((widget item-widget) (listing listing))
  (setf (slot-value listing 'active-widget) widget))

(defmethod (setf active-widget) ((widget qobject) (listing listing))
  (setf (active-widget listing) (item-widget widget listing)))

(defmethod (setf active-widget) ((place integer) (listing listing))
  (setf (active-widget listing) (widget place listing)))

(defmethod active-item ((listing listing))
  (when (active-widget listing)
    (widget-item (active-widget listing))))

(defmethod (setf active-item) (item (listing listing))
  (setf (active-widget listing) (item-widget item listing)))

(defmethod coerce-item (item (listing listing))
  (make-instance 'listing-item :item item :container listing))


(define-widget listing-item (QWidget item-widget draggable repaintable)
  ((active :initform NIL :accessor active-p)))

(define-initializer (listing-item setup)
  (setf (widget-item listing-item) (widget-item listing-item)))

(defmethod (setf widget-item) :before (item (listing-item listing-item))
  (when (typep (widget-item listing-item) 'qobject)
    (setf (parent (widget-item listing-item)) NIL)))

(define-override (listing-item paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter listing-item)))
    (when active
      (setf (q+:brush painter) (q+:highlight (q+:palette listing-item)))
      (q+:draw-rect painter (q+:rect listing-item)))
    (when (stringp (widget-item listing-item))
      (q+:draw-text painter (q+:rect listing-item)
                    (logior (q+:qt.align-left)
                            (q+:qt.align-vcenter))
                    (widget-item listing-item))))
  (stop-overriding))

(define-override (listing-item resize-event) (ev)
  (update listing-item)
  (stop-overriding))

(define-override (listing-item event) (ev)
  (when (= (enum-value (q+:type ev)) (q+:qevent.layout-request))
    (update listing-item))
  (stop-overriding))

(define-override (listing-item minimum-height) ()
  (if (typep (widget-item listing-item) 'qobject)
      (q+:minimum-height (widget-item listing-item))
      30))

(defmethod drag-start ((listing-item listing-item) x y)
  (declare (ignore x y))
  (setf (active-widget (container listing-item)) listing-item))

(defmethod update ((listing-item listing-item))
  (when (typep (widget-item listing-item) 'qobject)
    (setf (q+:geometry (widget-item listing-item)) (q+:geometry listing-item))))

(defmethod (setf active-p) :after (value (listing-item listing-item))
  (signal! listing-item (repaint))
  (setf (active-widget (container listing-item)) listing-item))

(defmethod (setf active-widget) :before (widget (listing listing))
  (when (active-widget listing)
    (setf (active-p (active-widget listing)) NIL)))

(defmethod (setf active-widget) :after ((listing-item listing-item) (listing listing))
  (setf (active-p listing-item) T))

(defmethod (setf active-widget) :around ((listing-item listing-item) (listing listing))
  (unless (eq listing-item (active-widget listing))
    (call-next-method)))
