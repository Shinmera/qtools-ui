#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defgeneric map-widgets (function container))
(defgeneric map-items (function item-container))
(defgeneric ensure-widget-order (container))

(defmacro do-widgets ((widget container &optional result) &body body)
  `(block NIL
     (map-widgets (lambda (,widget) ,@body) ,container)
     ,result))

(defmacro do-items ((item container &optional result) &body body)
  `(block NIL
     (map-items (lambda (,item) ,@body) ,container)
     ,result))

(define-widget container (QWidget layout)
  ((widgets :initform () :accessor widgets)))

(defmethod initialize-instance :after ((container container) &key widgets &allow-other-keys)
  (add-widget widgets container))

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

(defmethod widget-at-point ((point qobject) (container container))
  (do-widgets (widget container)
    (when (q+:contains (q+:geometry widget) point)
      (return widget))))

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

(defmethod clear-layout ((container container))
  (loop for widget = (pop (widgets container))
        while widget do (setf (parent widget) NIL))
  container)

(defmethod swap-widgets ((a integer) (b integer) (container container))
  (swapcar a b (widgets container))
  container)

(defmethod swap-widgets (a b (container container))
  (swap-widgets (widget-position a container) (widget-position b container) container))

(defmethod map-widgets (function (container container))
  (dolist (widget (widgets container) container)
    (funcall function widget)))

(defmethod ensure-widget-order ((container container))
  container)


(define-widget sorted-container (QWidget container)
  ((sorting :initarg :sorting :accessor sorting))
  (:default-initargs :sorting NIL))

(define-initializer (sorted-container setup)
  (setf (sorting sorted-container) (sorting sorted-container)))

(defmethod ensure-widget-order ((sorted-container sorted-container))
  (when (sorting sorted-container)
    (setf (widgets sorted-container) (stable-sort (widgets sorted-container) (sorting sorted-container))))
  sorted-container)

(defmethod (setf widget) :after (widget place (sorted-container sorted-container))
  (ensure-widget-order sorted-container))

(defmethod add-widget :after (widget (sorted-container sorted-container))
  (ensure-widget-order sorted-container))

(defmethod insert-widget :after (widget place (sorted-container sorted-container))
  (ensure-widget-order sorted-container))

(defmethod swap-widgets :after (a b (sorted-container sorted-container))
  (ensure-widget-order sorted-container))


(define-widget item-container (QWidget item-layout container)
  ())

(defmethod initialize-instance :after ((container container) &key items &allow-other-keys)
  (dolist (item items)
    (add-item item container)))

(defmethod map-items (function (item-container item-container))
  (map-widgets (lambda (widget)
                 (funcall function (widget-item widget)))
               item-container))


(define-widget sorted-item-container (QWidget sorted-container item-layout)
  ())

(defmethod ensure-widget-order ((sorted-item-container sorted-item-container))
  (when (sorting sorted-item-container)
    (setf (widgets sorted-item-container) (stable-sort (widgets sorted-item-container) (sorting sorted-item-container) :key #'widget-item)))
  sorted-item-container)

(defmethod (setf sorting) ((sorting (eql T)) (sorted-item-container sorted-item-container))
  (setf (sorting sorted-item-container) #'item<))

(defmethod (setf item-at) :after (item place (sorted-item-container sorted-item-container))
  (ensure-widget-order sorted-item-container))

(defmethod swap-items :after (a b (sorted-item-container sorted-item-container))
  (ensure-widget-order sorted-item-container))

(defmethod swap-items-at :after (a b (sorted-item-container sorted-item-container))
  (ensure-widget-order sorted-item-container))
