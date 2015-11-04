#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defgeneric repaint (repaintable))

(define-widget repaintable (QWidget)
  ())

(define-signal (repaintable repaint) ())

(define-initializer (repaintable setup)
  (connect! repaintable (repaint) repaintable (update)))

(defmethod repaint ((repaintable repaintable))
  (signal! repaintable (repaint)))
