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

(define-widget selectable-layout (QWidget item-layout)
  ((active-widget :initarg :active-widget :accessor active-widget)
   (selectable :initarg :selectable :accessor selectable))
  (:default-initargs
    :active-widget NIL
    :selectable T))

(define-widget selectable-item (QWidget item-widget mouse-propagator)
  ((active :initform NIL :accessor active-p)))

(defmethod (setf active-widget) (widget (selectable-layout selectable-layout))
  (error "~a is not a selectable-item." widget))

(defmethod (setf active-widget) :around (widget (selectable-layout selectable-layout))
  (unless (eq widget (active-widget selectable-layout))
    (call-next-method)))

(defmethod (setf active-widget) ((null null) (selectable-layout selectable-layout))
  (when (active-widget selectable-layout)
    (setf (active-p (active-widget selectable-layout)) NIL))
  (setf (slot-value selectable-layout 'active-widget) NIL))

(defmethod (setf active-widget) ((selectable-item selectable-item) (selectable-layout selectable-layout))
  (when (active-widget selectable-layout)
    (setf (active-p (active-widget selectable-layout)) NIL))
  (when (selectable selectable-layout)
    (setf (slot-value selectable-layout 'active-widget) selectable-item)
    (setf (active-p selectable-item) T)))

(defmethod (setf active-widget) ((widget qobject) (selectable-layout selectable-layout))
  (setf (active-widget selectable-layout) (item-widget widget selectable-layout)))

(defmethod (setf active-widget) ((place integer) (selectable-layout selectable-layout))
  (setf (active-widget selectable-layout) (widget place selectable-layout)))

(defmethod active-item ((selectable-layout selectable-layout))
  (when (active-widget selectable-layout)
    (widget-item (active-widget selectable-layout))))

(defmethod (setf active-item) (item (selectable-layout selectable-layout))
  (setf (active-widget selectable-layout) (item-widget item selectable-layout)))

(defmethod (setf active-p) :after (value (selectable-item selectable-item))
  (signal! selectable-item (repaint))
  (setf (active-widget (container selectable-item)) selectable-item))

(defmethod widget-acceptable-p ((selectable-item selectable-item) (selectable-layout selectable-layout))
  T)
