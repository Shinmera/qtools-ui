#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(named-readtables:in-readtable :qtools)

(defgeneric maximum (slider))
(defgeneric (setf maximum) (maximum slider))
(defgeneric minimum (slider))
(defgeneric (setf minimum) (minimum slider))
(defgeneric stepping (slider))
(defgeneric (setf stepping) (stepping slider))
(defgeneric default (slider))
(defgeneric (setf default) (default slider))

(define-widget double-slider (QSlider input)
  ((maximum :initarg :maximum :accessor maximum)
   (minimum :initarg :minimum :accessor minimum)
   (stepping :initarg :stepping :accessor stepping)
   (div))
  (:default-initargs
    :maximum 100.0 :minimum 1.0 :stepping 1.0))

(define-initializer (double-slider setup)
  (setf div (let ((str (string-trim "0" (format NIL "~f" stepping))))
              (expt 10 (- (length str) (position #\. str) 1))))
  (setf (q+:maximum double-slider) (round (* div maximum)))
  (setf (q+:minimum double-slider) (round (* div minimum)))
  (setf (q+:tick-interval double-slider) (round (* div stepping)))
  (setf (q+:orientation double-slider) (q+:qt.horizontal)))

(define-signal (double-slider value-changed) (double))

(define-slot (double-slider update) ((value int))
  (declare (connected double-slider (value-changed int)))
  (signal! double-slider (value-changed double) (/ value div)))

(defmethod value ((double-slider double-slider))
  (/ (q+:value double-slider) (slot-value double-slider 'div)))

(defmethod (setf value) (value (double-slider double-slider))
  (with-slots-bound (double-slider double-slider)
    (unless (<= minimum value maximum)
      (error "~a is not within [~a, ~a]." value minimum maximum))
    (setf (q+:value double-slider) (round (* value div)))))

(defmethod (setf maximum) :after (value (double-slider double-slider))
  (setf (q+:maximum double-slider) value))

(defmethod (setf minimum) :after (value (double-slider double-slider))
  (setf (q+:minimum double-slider) value))

(defmethod (setf stepping) :after (value (double-slider double-slider))
  (setf (q+:tick-interval double-slider) (round (* (slot-value double-slider 'div) value))))

(define-widget slider (QWidget input)
  ((maximum :initarg :maximum :accessor maximum)
   (minimum :initarg :minimum :accessor minimum)
   (stepping :initarg :stepping :accessor stepping)
   (default :initarg :default :accessor default))
  (:default-initargs
    :maximum 100.0 :minimum 0.0 :stepping 1.0 :default 0.0))

(define-signal (slider value-changed) (double))

(define-initializer (slider setup)
  (setf (q+:minimum-height slider) 20)
  (setf (q+:minimum-width slider) 100)
  (setf (q+:maximum-height slider) 40))

(define-subwidget (slider double-slider) (make-instance 'double-slider :maximum maximum :minimum minimum :stepping stepping)
  (setf (value double-slider) (or default minimum)))

(define-subwidget (slider spin-box) (q+:make-qdoublespinbox)
  (setf (q+:single-step spin-box) stepping)
  (setf (q+:maximum spin-box) maximum)
  (setf (q+:minimum spin-box) minimum)
  (setf (q+:value spin-box) (or default minimum))
  (setf (q+:fixed-width spin-box) 70))

(define-subwidget (slider button) (q+:make-qpushbutton)
  (setf (q+:text button) (princ-to-string default))
  (setf (q+:fixed-width button) 50)
  (setf (q+:visible button) (not (null default))))

(define-subwidget (slider layout) (q+:make-qhboxlayout slider)
  (setf (q+:spacing layout) 0)
  (setf (q+:contents-margins layout) (values 0 0 0 0))
  (q+:add-widget layout double-slider 8)
  (q+:add-widget layout spin-box 1)
  (q+:add-widget layout button 1))

(define-override (slider update) ()
  (q+:update double-slider)
  (q+:update spin-box)
  (q+:update button)
  (stop-overriding))

(define-slot (slider update) ((value double))
  (declare (connected double-slider (value-changed double)))
  (declare (connected spin-box (value-changed double)))
  (when (or (/= (value double-slider) value)
            (/= (value spin-box) value))
    (setf (value slider) value)
    (signal! slider (value-changed double) value)))

(define-slot (slider reset) ()
  (declare (connected button (clicked)))
  (setf (value double-slider) default)
  (setf (value spin-box) default))

(defmethod value ((slider slider))
  (q+:value (slot-value slider 'spin-box)))

(defmethod (setf value) (value (slider slider))
  (with-slots-bound (slider slider)
    (unless (<= minimum value maximum)
      (error "~a is not within [~a, ~a]." value minimum maximum))
    (setf (value spin-box) value)
    (setf (value double-slider) value)))

(defmethod (setf maximum) :after (value (slider slider))
  (with-slots-bound (slider slider)
    (setf (maximum double-slider) value)
    (setf (q+:maximum spin-box) value)
    (when default (setf (default slider) (min value default)))
    (setf (value slider) (min (value slider) value))))

(defmethod (setf minimum) :after (value (slider slider))
  (with-slots-bound (slider slider)
    (setf (minimum double-slider) value)
    (setf (q+:minimum spin-box) value)
    (when default (setf (default slider) (max value default)))
    (setf (value slider) (max (value slider) value))))

(defmethod (setf stepping) :after (value (slider slider))
  (with-slots-bound (slider slider)
    (setf (stepping double-slider) value)
    (setf (q+:single-step spin-box) stepping)))

(defmethod (setf default) :after (value (slider slider))
  (with-slots-bound (slider slider)
    (unless (<= minimum value maximum)
      (error "~a is not within [~a, ~a]." value minimum maximum))
    (setf (q+:text button) (princ-to-string value))
    (setf (q+:visible button) (not (null value)))))
