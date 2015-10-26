#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget container (QWidget)
  ((widgets :initarg :widgets :accessor widgets))
  (:default-initargs :widgets ()))

(defgeneric widget (place container))
(defgeneric widget-position (widget container))
(defgeneric add-widget (widget container))
(defgeneric (setf widget) (widget place container))
(defgeneric insert-widget (widget place container))
(defgeneric remove-widget (place container))
(defgeneric swap-widget (a b container))
(defgeneric update (container))

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

(defmethod (setf widget) :around (widget place (container container))
  (call-next-method)
  (update container))

(defmethod widget-position ((widget qobject) (container container))
  (position widget (widgets container)))

(defmethod widget-position ((n integer) (container container))
  (unless (< -1 n (length (widgets container)))
    (error "~a is out of bounds for ~a." n container))
  n)

(defmethod add-widget ((widget qobject) (container container))
  (push widget (widgets container))
  (setf (parent widget) container))

(defmethod add-widget :around (widget (container container))
  (call-next-method)
  (update container))

(defmethod insert-widget ((widget qobject) (n integer) (container container))
  (insert widget n (widgets container))
  (setf (parent widget) container))

(defmethod insert-widget ((widget qobject) (find qobject) (container container))
  (insert-widget widget (position widget (widgets container)) container))

(defmethod insert-widget :around (widget place (container container))
  (call-next-method)
  (update container))

(defmethod remove-widget ((n integer) (container container))
  (let ((widget (remove-nth n (widgets container))))
    (setf (parent widget) NIL)
    widget))

(defmethod remove-widget ((widget qobject) (container container))
  (remove-widget (position widget (widgets container)) container))

(defmethod remove-widget :around (place (container container))
  (call-next-method)
  (update container))

(defmethod swap-widget ((a integer) (b integer) (container container))
  (swapcar a b (widgets container)))

(defmethod swap-widget (a b (container container))
  (swap-widget (widget-position a container) (widget-position b container) container))

(defmethod swap-widget :around (a b (container container))
  (call-next-method)
  (update container))

(defmethod update ((container container)))
