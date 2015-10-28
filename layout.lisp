#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget layout (QWidget)
  ())

(defgeneric widget (place container))
(defgeneric widget-position (widget container))
(defgeneric add-widget (widget container))
(defgeneric (setf widget) (widget place container))
(defgeneric insert-widget (widget place container))
(defgeneric remove-widget (place container))
(defgeneric swap-widget (a b container))
(defgeneric update (container))

(define-override (layout resize-event) (ev)
  (update layout)
  (stop-overriding))

(define-override (layout event) (ev)
  (when (= (enum-value (q+:type ev)) (q+:qevent.layout-request))
    (update layout))
  (stop-overriding))

(defmethod update ((layout layout)))
