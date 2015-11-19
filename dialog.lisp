#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget dialog (QDialog)
  ())

(defgeneric show (dialog &key modal &allow-other-keys)
  (:method ((dialog dialog) &key (modal T))
    (= (if modal
           (q+:exec dialog)
           (q+:open dialog))
       (q+:qdialog.accepted))))

(define-widget simple-input-dialog (QDialog dialog storing-input)
  ())

(defmethod show :around ((simple-input-dialog simple-input-dialog) &key)
  (let ((prev (value simple-input-dialog))
        (accepted (call-next-method)))
    (unless accepted
      (setf (value simple-input-dialog) prev))
    accepted))

(define-subwidget (simple-input-dialog ok-button) (q+:make-qpushbutton "&Ok")
  (connect! ok-button (clicked) simple-input-dialog (accept)))

(define-subwidget (simple-input-dialog cancel-button) (q+:make-qpushbutton "&Cancel")
  (connect! cancel-button (clicked) simple-input-dialog (reject)))

(define-subwidget (simple-input-dialog dialog-buttons) (q+:make-qhboxlayout)
  (q+:add-stretch dialog-buttons 1)
  (q+:add-widget dialog-buttons ok-button)
  (q+:add-widget dialog-buttons cancel-button)
  (q+:add-stretch dialog-buttons 1))
