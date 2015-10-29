#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget container (QWidget layout)
  ((widgets :initarg :widgets :accessor widgets))
  (:default-initargs :widgets ()))

(defmethod widget ((n integer) (container container))
  (nth n (widgets container)))

(defmethod widget ((widget qobject) (container container))
  (find widget (widgets container)))

(defmethod (setf widget) ((widget qobject) (n integer) (container container))
  (setf (nth n (widgets container)) widget))

(defmethod (setf widget) ((widget qobject) (find qobject) (container container))
  (loop for cell on (widgets container)
        when (eql (car cell) find)
        do (return (setf (car cell) widget))))

(defmethod widget-position (widget (container container) &key key test test-not)
  (position widget (widgets container) :key key :test test :test-not test-not))

(defmethod find-widget (widget (container container) &key key test test-not)
  (find widget (widgets container) :key key :test test :test-not test-not))

(defmethod add-widget ((widget qobject) (container container))
  (push widget (widgets container))
  (setf (parent widget) container)
  widget)

(defmethod insert-widget ((widget qobject) (n integer) (container container))
  (insert widget n (widgets container))
  (setf (parent widget) container)
  widget)

(defmethod insert-widget ((widget qobject) (find qobject) (container container))
  (insert-widget widget (position widget (widgets container)) container))

(defmethod remove-widget ((n integer) (container container))
  (let ((widget (remove-nth n (widgets container))))
    (setf (parent widget) NIL)
    widget))

(defmethod remove-widget ((widget qobject) (container container))
  (remove-widget (position widget (widgets container)) container))

(defmethod swap-widgets ((a integer) (b integer) (container container))
  (swapcar a b (widgets container))
  container)

(defmethod swap-widgets (a b (container container))
  (swap-widgets (widget-position a container) (widget-position b container) container))
