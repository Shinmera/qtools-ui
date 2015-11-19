#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget option (QWidget input)
  ((target :initarg :target :accessor target)
   (reader :initarg :reader :accessor reader)
   (writer :initarg :writer :accessor writer)
   (title :initarg :title :accessor title))
  (:default-initargs
    :target (error "TARGET required.")
    :reader (error "READER required.")))

(define-initializer (option setup)
  (unless (slot-boundp option 'writer)
    (setf (writer option) `(setf ,(reader option))))
  (unless (slot-boundp option 'title)
    (setf (title option) (string (reader option))))
  (setf (value option) (funcall (fdefinition (reader option)) (target option))))

(define-slot (option input-done) ()
  (declare (connected option (input-done)))
  (funcall (fdefinition (writer option)) (value option) (target option)))

(define-widget string-option (QLineEdit option)
  ())

(define-initializer (string-option setup)
  (connect! string-option (editing-finished) string-option (input-done))
  (connect! string-option (text-changed string) string-option (input-updated)))

(defmethod value ((string-option string-option))
  (q+:text string-option))

(defmethod (setf value) (value (string-option string-option))
  (setf (q+:text string-option) (princ-to-string value)))

(define-widget text-option (QPlainTextEdit option)
  ())

(define-initializer (text-option setup)
  (connect! text-option (text-changed) text-option (input-updated)))

(define-override (text-option focus-out-event) (ev)
  (signal! text-option (input-done))
  (stop-overriding))

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

(define-initializer (small-double-option setup)
  (connect! small-double-option (editing-finished) small-double-option (input-done))
  (connect! small-double-option (value-changed double) small-double-option (input-updated)))

(define-widget small-color-option (QPushButton option)
  ((dialog :initform (make-instance 'color-picker) :finalized T)))

(define-slot (small-color-option pressed) ()
  (declare (connected small-color-option (clicked)))
  (when (show dialog)
    (repaint small-color-option)))

(define-override (small-color-option paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter small-color-option)))
    (q+:fill-rect painter (q+:rect small-color-option) (value dialog))))

(defmethod value ((small-color-option small-color-option))
  (value (slot-value small-color-option 'dialog)))

(defmethod (setf value) (value (small-color-option small-color-option))
  (setf (value (slot-value small-color-option 'dialog)) value))
