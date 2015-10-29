#|
This file is a part of Qtools-UI
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defgeneric active-widget (listing))
(defgeneric (setf active-widget) (widget listing))
(defgeneric active-item (listing))
(defgeneric (setf active-item) (item listing))

(define-widget listing (QWidget container item-layout)
  ((active-widget :initarg :active-widget :accessor active-widget)
   (minimum-row-height :initarg :minimum-row-height :accessor minimum-row-height)
   (fixed-row-height :initarg :fixed-row-height :accessor fixed-row-height))
  (:default-initargs
    :active-widget NIL
    :minimum-row-height 20
    :fixed-row-height NIL))

(defmethod update ((listing listing))
  (loop for y = 0 then (+ y height)
        for widget in (widgets listing)
        for hint = (q+:minimum-size-hint widget)
        for height = (or (fixed-row-height listing)
                         (max (minimum-row-height listing) (q+:minimum-height widget) (q+:height hint)))
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

(defmethod widget-acceptable-p ((widget qobject) (listing listing))
  NIL)

(define-widget listing-item (QWidget item-widget draggable repaintable mouse-propagator)
  ((active :initform NIL :accessor active-p)))

(define-initializer (listing-item setup)
  (setf (widget-item listing-item) (widget-item listing-item)))

(defmethod (setf widget-item) :before (item (listing-item listing-item))
  (when (typep (widget-item listing-item) 'qobject)
    (setf (parent (widget-item listing-item)) NIL)
    (q+:remove-event-filter (widget-item listing-item) listing-item)))

(defmethod (setf widget-item) :after ((item qobject) (listing-item listing-item))
  (v:info :test "INSTALL")
  (q+:install-event-filter item listing-item))

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
      (max 20 (q+:minimum-height (widget-item listing-item)))
      30))

(define-override (listing-item size-hint) ()
  (if (typep (widget-item listing-item) 'qobject)
      (q+:size-hint (widget-item listing-item))
      (call-next-qmethod)))

(define-override (listing-item minimum-size-hint) ()
  (if (typep (widget-item listing-item) 'qobject)
      (q+:minimum-size-hint (widget-item listing-item))
      (call-next-qmethod)))

(define-override (listing-item set-geometry) (&rest args)
  (declare (ignore args))
  (update listing-item)
  (call-next-qmethod))

(defmethod drag-start ((listing-item listing-item) x y)
  (declare (ignore x y))
  (setf (active-widget (container listing-item)) listing-item))

(defmethod drag ((listing-item listing-item) px py nx ny)
  (let* ((pos (q+:map-to-global listing-item (q+:make-qpoint nx ny)))
         (widget (q+:qapplication-widget-at pos)))
    (when (and (typep widget 'listing-item)
               (eql (container widget) (container listing-item))
               (not (eql widget listing-item)))
      (swap-widgets widget listing-item (container listing-item)))))

(defmethod update ((listing-item listing-item))
  (when (typep (widget-item listing-item) 'qobject)
    (setf (q+:geometry (widget-item listing-item))
          (values 3 3 (- (q+:width listing-item) 6) (- (q+:height listing-item) 6)))))

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

(defmethod widget-acceptable-p ((listing-item listing-item) (listing listing))
  T)
