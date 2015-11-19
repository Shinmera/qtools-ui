#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget panel-main-window (QMainWindow)
  ())

(define-subwidget (panel-main-window sidebar) (make-instance 'panel-container))

(define-subwidget (panel-main-window layout) (make-instance 'compass)
  (setf (q+:central-widget panel-main-window) layout)
  (setf (widget :east layout) sidebar))

(defun pmw-target-layout (place panel-main-window)
  (if (keywordp place)
      (slot-value panel-main-window 'layout)
      (slot-value panel-main-window 'sidebar)))

(defmethod (setf widget) (widget place (panel-main-window panel-main-window))
  (setf (widget place (pmw-target-layout place panel-main-window)) widget))

(defmethod widget (place (panel-main-window panel-main-window))
  (widget place (pmw-target-layout place panel-main-window)))

(defmethod find-widget (widget (panel-main-window panel-main-window) &rest args)
  (apply #'find-widget widget (slot-value panel-main-window 'sidebar) args))

(defmethod widget-position (widget (panel-main-window panel-main-window) &rest args)
  (apply #'widget-position widget (slot-value panel-main-window 'sidebar) args))

(defmethod widget-at-point (point (panel-main-window panel-main-window))
  (or (widget-at-point point (slot-value panel-main-window 'sidebar))
      (widget-at-point point (slot-value panel-main-window 'layout))))

(defmethod add-widget (widget (panel-main-window panel-main-window))
  (add-widget widget (slot-value panel-main-window 'sidebar)))

(defmethod insert-widget (widget place (panel-main-window panel-main-window))
  (insert-widget widget place (pmw-target-layout place panel-main-window)))

(defmethod remove-widget (place (panel-main-window panel-main-window))
  (remove-widget place (pmw-target-layout place panel-main-window)))

(defmethod swap-widgets (a b (panel-main-window panel-main-window))
  (swap-widgets a b (slot-value panel-main-window 'sidebar)))

(defmethod clear-layout ((panel-main-window panel-main-window))
  (clear-layout (slot-value panel-main-window 'sidebar))
  (clear-layout (slot-value panel-main-window 'layout)))

(defmethod update ((panel-main-window panel-main-window))
  (update (slot-value panel-main-window 'sidebar))
  (update (slot-value panel-main-window 'layout)))

(defmethod widget-acceptable-p (widget (panel-main-window panel-main-window))
  (widget-acceptable-p widget (slot-value panel-main-window 'sidebar)))
