#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget option (QWidget)
  ((target :initarg :target :accessor target)
   (reader :initarg :reader :accessor reader)
   (writer :initarg :writer :accessor writer)
   (title :initarg :title :accessor title))
  (:default-initargs
    :target (error "TARGET required.")
    :reader (error "READER required.")))

(define-signal (option option-changed) ())

(define-initializer (option setup)
  (unless (slot-boundp option 'writer)
    (setf (writer option) `(setf ,(reader option))))
  (unless (slot-boundp option 'title)
    (setf (title option) (string (reader option))))
  (setf (value option) (funcall (fdefinition (reader option)) (target option))))

(define-slot (option option-changed) ()
  (declare (connected option (option-changed)))
  (funcall (fdefinition (writer option)) (value option) (target option)))

(defmethod (setf value) :after (value (option option))
  (signal! option (option-changed)))

(define-widget string-option (QLineEdit option)
  ())

(define-initializer (string-option setup)
  (connect! string-option (editing-finished) string-option (option-changed)))

(defmethod value ((string-option string-option))
  (q+:text string-option))

(defmethod (setf value) (value (string-option string-option))
  (setf (q+:text string-option) (princ-to-string value)))

(define-widget text-option (QPlainTextEdit option)
  ())

(define-initializer (text-option setup)
  (connect! text-option (text-changed) text-option (option-changed)))

(defmethod value ((text-option text-option))
  (q+:to-plain-text text-option))

(defmethod (setf value) (value (text-option text-option))
  (setf (q+:plain-text text-option) (princ-to-string value)))

(define-widget double-option (QWidget slider option)
  ())

(define-widget color-option (QWidget color-triangle option)
  ())

(define-widget small-double-option (QDoubleSpinBox option)
  ())

(define-slot (small-double-option value-changed) ((value double))
  (declare (connected small-double-option (value-changed double)))
  (signal! small-double-option (option-changed)))

(define-widget small-color-option (QPushButton option)
  ())

(define-slot (small-color-option pressed) ()
  (declare (connected small-color-option (clicked)))
  ())

(defmethod value ((small-color-option small-color-option))
  (q+:color (q+:palette small-color-option) (q+:qpalette.button)))

(defmethod (setf value) (value (small-color-option small-color-option))
  (setf (q+:color (q+:palette small-color-option) (q+:qpalette.button))
        (coerce-color value)))
