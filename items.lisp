#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defgeneric widget-item (item-widget))
(defgeneric (setf widget-item) (item item-widget))
(defgeneric item-widget (item layout))
(defgeneric coerce-item (item layout))
(defgeneric item-at (place layout))
(defgeneric (setf item-at) (item place layout))
(defgeneric item-position (item layout &key key test test-not))
(defgeneric find-item (item layout &key key test test-not))
(defgeneric add-item (item layout))
(defgeneric insert-item (item place layout))
(defgeneric remove-item (item layout))
(defgeneric remove-item-at (place layout))
(defgeneric swap-items (a b layout))
(defgeneric swap-items-at (a b layout))
(defgeneric item-acceptable-p (item layout))
(defgeneric item< (a b))
(defgeneric item= (a b))
(defgeneric item> (a b))
(defgeneric item<= (a b))
(defgeneric item>= (a b))

(define-widget item-layout (QWidget layout)
  ())

(define-widget item-widget (QWidget)
  ((item :initarg :item :accessor widget-item)
   (container :initarg :container :accessor container))
  (:default-initargs :item NIL))

(define-print-method (instance item-widget stream)
  (print-unreadable-object (instance stream :type T :identity T)
    (format stream "~s ~a" :item (widget-item instance))))

(defmethod (setf widget-item) ((widget qobject) (item-widget item-widget))
  (setf (parent widget) item-widget)
  (setf (slot-value item-widget 'item) widget))

(defmethod item-widget (item (layout item-layout))
  (find-widget item layout :key #'widget-item))

(defmethod coerce-item :around (item (layout item-layout))
  (unless (item-acceptable-p item layout)
    (cerror "Add the item anyway." "~a does not accept ~a." layout item))
  (call-next-method))

(defmethod coerce-item (item (layout item-layout))
  (make-instance 'item-widget :item item :container layout))

(defmethod item-at (place (layout item-layout))
  (let ((widget (widget place layout)))
    (when widget (widget-item widget))))

(defmethod (setf item-at) (item place (layout item-layout))
  (let ((widget (widget place layout)))
    (if widget
        (setf (widget-item widget) item)
        (setf (widget place layout) (coerce-item item layout)))))

(defmethod item-position :around (item (layout item-layout) &key key test test-not)
  (when (and test test-not)
    (error "Cannot specify both TEST and TEST-NOT simultaneously."))
  (call-next-method item layout :key key :test (default-test test test-not) :test-not test-not))

(defmethod item-position (item (layout item-layout) &key key test test-not)
  (widget-position item layout :key (lambda (widget) (funcall key (widget-item widget))) :test test :test-not test-not))

(defmethod find-item :around (item (layout item-layout) &key key test test-not)
  (when (and test test-not)
    (error "Cannot specify both TEST and TEST-NOT simultaneously."))
  (call-next-method item layout :key key :test (default-test test test-not) :test-not test-not))

(defmethod find-item (item (layout item-layout) &key key test test-not)
  (find-widget item layout :key (lambda (widget) (funcall key (widget-item widget))) :test test :test-not test-not))

(defmethod add-item (item (layout item-layout))
  (add-widget (coerce-item item layout) layout))

(defmethod insert-item (item place (layout item-layout))
  (insert-widget (coerce-item item layout) place layout))

(defmethod remove-item (item (layout item-layout))
  (remove-widget (item-widget item layout) layout))

(defmethod remove-item-at (place (layout item-layout))
  (let ((widget (remove-widget place layout)))
    (when widget (widget-item widget))))

(defmethod swap-items (a b (layout item-layout))
  (swap-widgets (item-widget a layout) (item-widget b layout) layout))

(defmethod swap-items-at (a b (layout item-layout))
  (swap-widgets a b layout))

(defmethod item-acceptable-p (item (layout item-layout))
  T)

(defmethod item< ((a string) (b string))
  (string< a b))

(defmethod item< ((a number) (b number))
  (< a b))

(defmethod item= ((a string) (b string))
  (string= a b))

(defmethod item= ((a number) (b number))
  (= a b))

(defmethod item<= (a b)
  (or (item= a b) (item< a b)))

(defmethod item>= (a b)
  (not (item< a b)))

(defmethod item> (a b)
  (not (item<= a b)))
