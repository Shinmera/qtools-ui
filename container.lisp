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
(defgeneric sorting (sorted-container))
(defgeneric (setf sorting) (sorting sorted-container))
(defgeneric widgets (container))
(defgeneric (setf widgets) (widgets container))

(defmacro do-widgets ((widget container &optional result) &body body)
  `(block NIL
     (map-widgets (lambda (,widget) ,@body) ,container)
     ,result))

(defmacro do-items ((item container &optional result) &body body)
  `(block NIL
     (map-items (lambda (,item) ,@body) ,container)
     ,result))

(define-widget container (QWidget layout)
  ((widgets :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor widgets)))

(defmethod initialize-instance :after ((container container) &key widgets &allow-other-keys)
  (add-widget widgets container))

(defmethod widget ((n integer) (container container))
  (elt (widgets container) n))

(defmethod widget ((widget qobject) (container container))
  (find widget (widgets container)))

(defmethod (setf widget) ((widget qobject) (n integer) (container container))
  (setf (elt (widgets container) n) widget))

(defmethod (setf widget) ((widget qobject) (find qobject) (container container))
  (loop for i from 0 below (length (widgets container))
        when (eql find (aref (widgets container) i))
        do (return (setf (aref (widgets container) i) widget))))

(defmethod widget-position (widget (container container) &key key test test-not)
  (position widget (widgets container) :key key :test test :test-not test-not))

(defmethod find-widget (widget (container container) &key key test test-not)
  (find widget (widgets container) :key key :test test :test-not test-not))

(defmethod widget-at-point ((point qobject) (container container))
  (do-widgets (widget container)
    (when (q+:contains (q+:geometry widget) point)
      (return widget))))

(defmethod add-widget ((widget qobject) (container container))
  (vector-push-extend widget (widgets container))
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

(defmethod clear-layout ((container container) &optional finalize)
  (do-widgets (widget container)
    (if finalize
        (finalize widget)
        (setf (parent widget) NIL)))
  (setf (fill-pointer (widgets container)) 0)
  container)

(defmethod swap-widgets ((a integer) (b integer) (container container))
  (rotatef (elt (widgets container) a) (elt (widgets container) b))
  container)

(defmethod swap-widgets (a b (container container))
  (swap-widgets (widget-position a container) (widget-position b container) container))

(defmethod map-widgets (function (container container))
  (loop for widget across (widgets container)
        do (funcall function widget)))

(defmethod ensure-widget-order ((container container))
  container)


(define-widget sorted-container (QWidget container)
  ((sorting :initarg :sorting :accessor sorting))
  (:default-initargs :sorting NIL))

(define-initializer (sorted-container setup)
  (setf (sorting sorted-container) (sorting sorted-container)))

(defmethod ensure-widget-order ((sorted-container sorted-container))
  (when (sorting sorted-container)
    (stable-sort-into (widgets sorted-container) (sorting sorted-container)))
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
    (stable-sort-into (widgets sorted-item-container) (sorting sorted-item-container) :key #'widget-item))
  sorted-item-container)

(defmethod (setf sorting) ((sorting (eql T)) (sorted-item-container sorted-item-container))
  (setf (sorting sorted-item-container) #'item<))

(defmethod (setf item-at) :after (item place (sorted-item-container sorted-item-container))
  (ensure-widget-order sorted-item-container))

(defmethod swap-items :after (a b (sorted-item-container sorted-item-container))
  (ensure-widget-order sorted-item-container))

(defmethod swap-items-at :after (a b (sorted-item-container sorted-item-container))
  (ensure-widget-order sorted-item-container))
