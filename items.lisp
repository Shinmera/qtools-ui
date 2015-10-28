#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defgeneric widget-item (item-widget))
(defgeneric item-widget (item layout))
(defgeneric coerce-item (item layout))
(defgeneric item-at (place layout))
(defgeneric (setf item) (item place layout))
(defgeneric item-position (item layout &key key test test-not))
(defgeneric find-item (item layout &key key test test-not))
(defgeneric add-item (item layout))
(defgeneric insert-item (item place layout))
(defgeneric remove-item (item layout))
(defgeneric remove-item-at (place layout))
(defgeneric swap-items (a b layout))
(defgeneric item-acceptable-p (item layout))

(define-widget item-widget (QWidget)
  ((item :initarg :item :accessor widget-item)
   (container :initarg :container :accessor container))
  (:default-initargs :item NIL))

(defmethod item-widget (item layout)
  (find-widget item layout :key #'widget-item))

(defmethod coerce-item (item layout)
  (make-instance 'item-widget :item item :container layout))

(defmethod item-at (place layout)
  (widget-item (widget-at place layout)))

(defmethod (setf item) (item place layout)
  (let ((widget (widget place layout)))
    (if widget
        (setf (widget-item widget) item)
        (setf (widget place layout) (coerce-item item layout)))))

(defmethod item-position :around (item layout &key key test test-not)
  (when (and test test-not)
    (error "Cannot specify both TEST and TEST-NOT simultaneously."))
  (call-next-method widget layout :key (default-test test test-not) :test (or test #'eql) :test-not test-not))

(defmethod item-position (item layout &key key test test-not)
  (widget-position item layout :key (lambda (widget) (funcall key (widget-item widget))) :test test :test-not test-not))

(defmethod find-item :around (item layout &key key test test-not)
  (when (and test test-not)
    (error "Cannot specify both TEST and TEST-NOT simultaneously."))
  (call-next-method widget layout :key (default-test test test-not) :test (or test #'eql) :test-not test-not))

(defmethod find-item (item layout &key key test test-not)
  (find-widget item layout :key (lambda (widget) (funcall key (widget-item widget))) :test test :test-not test-not))

(defmethod add-item (item layout)
  (add-widget (coerce-item item layout) layout))

(defmethod insert-item (item place layout)
  (add-widget (coerce-item item layout) place layout))

(defmethod remove-item (item layout)
  (remove-widget (item-widget item layout) layout))

(defmethod swap-item (a b layout)
  (swap-widget (item-widget a layout) (item-widget b layout) layout))

(defmethod item-acceptable-p (item layout)
  NIL)

(defmethod item-acceptable-p (item (layout layout))
  T)
