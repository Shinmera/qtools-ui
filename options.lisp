#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defgeneric option-effective-target (option))
(defgeneric option-target-value (option))
(defgeneric (setf option-target-value) (value option))

(define-widget option (QWidget input)
  ((target :initarg :target :accessor target)
   (reader :initarg :reader :accessor reader)
   (writer :initarg :writer :accessor writer)
   (title :initarg :title :accessor title)
   (accessor-type :initarg :accessor-type :accessor accessor-type))
  (:default-initargs
    :target (error "TARGET required.")
    :reader (error "READER required.")
    :writer NIL
    :accessor-type :accessor))

(defmethod option-effective-target ((option option))
  (let ((target (target option)))
    (typecase target
      (cl:function (funcall target))
      (symbol (symbol-value target))
      (T target))))

(defmethod option-target-value ((option option))
  (let ((target (option-effective-target option)))
    (ecase (accessor-type option)
      (:accessor (funcall (reader option) target))
      (:slot (slot-value target (reader option)))
      (:function (funcall (reader option) target))
      (:value (value target))
      (:variable target))))

(defmethod (setf option-target-value) (value (option option))
  (let ((writer (or (writer option) (reader option)))
        (target (option-effective-target option)))
    (ecase (accessor-type option)
      (:accessor (funcall (fdefinition `(setf ,writer)) value target))
      (:slot (setf (slot-value target writer) value))
      (:function (funcall writer value target))
      (:value (setf (value target) value))
      (:variable (setf (symbol-value (target option)) value)))))

(define-initializer (option setup)
  (unless (slot-boundp option 'title)
    (setf (title option) (typecase (reader option)
                           (symbol (string (reader option)))
                           (T ""))))
  (setf (value option) (option-target-value option)))

(define-slot (option input-done) ()
  (declare (connected option (input-done)))
  (setf (option-target-value option) (value option)))

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
