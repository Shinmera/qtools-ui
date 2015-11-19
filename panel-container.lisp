#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defgeneric iconified-p (container))
(defgeneric (setf iconified-p) (value container))
(defgeneric iconify (container))
(defgeneric deiconify (container))

(define-widget panel-container (QScrollArea)
  ((orientation :initarg :orientation :accessor orientation)
   (iconified :initarg :iconified :accessor iconified-p))
  (:default-initargs
    :orientation :vertical))

(define-initializer (panel-container setup)
  (ecase orientation
    (:horizontal
     (setf (q+:horizontal-scroll-bar-policy panel-container)
           (q+:qt.scroll-bar-always-on))
     (setf (q+:vertical-scroll-bar-policy panel-container)
           (q+:qt.scroll-bar-always-off)))
    (:vertical
     (setf (q+:horizontal-scroll-bar-policy panel-container)
           (q+:qt.scroll-bar-always-off))
     (setf (q+:vertical-scroll-bar-policy panel-container)
           (q+:qt.scroll-bar-always-on))))
  (setf (q+:widget-resizable panel-container) T))

(define-override (panel-container size-hint) ()
  (q+:make-qsize 300 300))

(define-subwidget (panel-container viewport)
                  (make-instance 'splitter :orientation (orientation panel-container))
  (setf (q+:widget panel-container) viewport))

(defmethod widget (place (panel-container panel-container))
  (widget place (slot-value panel-container 'viewport)))

(defmethod (setf widget) (widget place (panel-container panel-container))
  (setf (widget place (slot-value panel-container 'viewport)) widget))

(defmethod add-widget (widget (panel-container panel-container))
  (add-widget widget (slot-value panel-container 'viewport)))

(defmethod insert-widget (widget place (panel-container panel-container))
  (insert-widget widget place (slot-value panel-container 'viewport)))

(defmethod remove-widget (place (panel-container panel-container))
  (remove-widget place (slot-value panel-container 'viewport)))

(defmethod update ((panel-container panel-container))
  (update (slot-value panel-container 'viewport)))
