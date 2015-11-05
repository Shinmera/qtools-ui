#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defgeneric widget (place layout))
(defgeneric (setf widget) (widget place layout))
(defgeneric find-widget (widget layout &key key test test-not))
(defgeneric widget-position (widget layout &key key test test-not))
(defgeneric widget-at-point (point layout))
(defgeneric add-widget (widget layout))
(defgeneric insert-widget (widget place layout))
(defgeneric remove-widget (place layout))
(defgeneric swap-widgets (a b layout))
(defgeneric clear-layout (layout))
(defgeneric update (layout))
(defgeneric widget-acceptable-p (widget layout))

(define-widget layout (QWidget)
  ())

(defmethod update ((layout layout)))

(defmethod find-widget :around (widget layout &key key test test-not)
  (when (and test test-not)
    (error "Cannot specify both TEST and TEST-NOT simultaneously."))
  (call-next-method widget layout :key (default-test test test-not) :test (or test #'eql) :test-not test-not))

(defmethod widget-position :around (widget layout &key key test test-not)
  (when (and test test-not)
    (error "Cannot specify both TEST and TEST-NOT simultaneously."))
  (call-next-method widget layout :key (default-test test test-not) :test (or test #'eql) :test-not test-not))

(defmethod widget-acceptable-p (widget (layout layout))
  NIL)

(defmethod widget-acceptable-p ((widget qobject) (layout layout))
  T)

(defmethod widget-at-point :around ((point qobject) (layout layout))
  (when (and (<= 0 (q+:x point) (q+:width layout))
             (<= 0 (q+:y point) (q+:height layout)))
    (call-next-method)))

(defmethod widget-at-point ((point cons) (layout layout))
  (widget-at-point (q+:make-qpointf (car point) (cdr point)) layout))

(defun check-widget-permitted (widget layout)
  (unless (widget-acceptable-p widget layout)
    (cerror "Add the widget anyway." "~a does not accept ~a." layout widget))
  (when (and widget (eql (parent widget) layout))
    (error "~a is already contained in ~a." widget layout)))

(defmethod (setf widget) :around (widget place (layout layout))
  (check-widget-permitted widget layout)
  (prog1 (call-next-method)
    (update layout)))

(defmethod add-widget :around (widget (layout layout))
  (check-widget-permitted widget layout)
  (prog1 (call-next-method)
    (update layout)))

(defmethod add-widget ((widgets list) (layout layout))
  (dolist (widget widgets widgets)
    (add-widget widget layout)))

(defmethod insert-widget :around (widget place (layout layout))
  (check-widget-permitted widget layout)
  (prog1 (call-next-method)
    (update layout)))

(defmethod insert-widget ((widgets list) place (layout layout))
  (dolist (widget widgets widgets)
    (insert-widget widget place layout)))

(defmethod remove-widget :around (place (layout layout))
  (prog1 (call-next-method)
    (update layout)))

(defmethod remove-widget ((widgets list) (layout layout))
  (dolist (widget widgets widgets)
    (remove-widget widget layout)))

(defmethod swap-widgets :around (a b (layout layout))
  (prog1 (call-next-method)
    (update layout)))

(define-override (layout resize-event) (ev)
  (update layout)
  (stop-overriding))

(define-override (layout event) (ev)
  (when (= (enum-value (q+:type ev)) (q+:qevent.layout-request))
    (update layout))
  (stop-overriding))
