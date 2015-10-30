#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget cell (QWidget selectable-item draggable repaintable mouse-propagator)
  ((padding :initarg :padding :accessor padding))
  (:default-initargs :padding 3))

(define-initializer (cell setup)
  (setf (widget-item cell) (widget-item cell)))

(defmethod (setf widget-item) :before (item (cell cell))
  (when (typep (widget-item cell) 'qobject)
    (setf (parent (widget-item cell)) NIL)
    (q+:remove-event-filter (widget-item cell) cell)))

(defmethod (setf widget-item) :after ((item qobject) (cell cell))
  (q+:install-event-filter item cell))

(define-override (cell paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter cell)))
    (when (active-p cell)
      (setf (q+:brush painter) (q+:highlight (q+:palette cell)))
      (q+:draw-rect painter (q+:rect cell)))
    (unless (typep (widget-item cell) 'qobject)
      (q+:draw-text painter (q+:rect cell)
                    (logior (q+:qt.align-left)
                            (q+:qt.align-vcenter))
                    (princ-to-string (widget-item cell)))))
  (stop-overriding))

(define-override (cell resize-event) (ev)
  (update cell)
  (stop-overriding))

(define-override (cell event) (ev)
  (when (= (enum-value (q+:type ev)) (q+:qevent.layout-request))
    (update cell))
  (stop-overriding))

(define-override (cell minimum-height) ()
  (if (typep (widget-item cell) 'qobject)
      (max 20 (+ padding (q+:minimum-height (widget-item cell)) padding))
      30))

(defun padded-hint (cell hint)
  (let ((padding (padding cell)))
    (if (q+:is-valid hint)
        (q+:make-qsize (+ padding (q+:width hint) padding)
                       (+ padding (q+:height hint) padding))
        hint)))

(define-override (cell size-hint) ()
  (cond ((typep (widget-item cell) 'qobject)
         (padded-hint cell (q+:size-hint (widget-item cell))))
        (T (call-next-qmethod))))

(define-override (cell minimum-size-hint) ()
  (cond ((typep (widget-item cell) 'qobject)
         (padded-hint cell (q+:minimum-size-hint (widget-item cell))))
        (T (call-next-qmethod))))

(define-override (cell set-geometry) (&rest args)
  (declare (ignore args))
  (update cell)
  (call-next-qmethod))

(defmethod drag-start ((cell cell) x y)
  (declare (ignore x y))
  (setf (active-widget (container cell)) cell))

(defmethod drag ((cell cell) px py nx ny)
  (declare (ignore px py))
  (let* ((pos (q+:map-to-parent cell (q+:make-qpoint nx ny)))
         (widget (widget-at-point pos (container cell))))
    (when (and (typep widget 'cell)
               (eql (container widget) (container cell))
               (not (eql widget cell)))
      (swap-widgets widget cell (container cell)))))

(defmethod update ((cell cell))
  (when (typep (widget-item cell) 'qobject)
    (let ((padding (padding cell)))
      (setf (q+:geometry (widget-item cell))
            (values padding padding
                    (- (q+:width cell) padding padding)
                    (- (q+:height cell) padding padding))))))
