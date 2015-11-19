#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget flow-layout (QWidget container)
  ())

(defmethod update ((flow-layout flow-layout))
  (when (widgets flow-layout)
    (let ((maxheight 0)
          (x 0) (y 0))
      (do-widgets (widget flow-layout)
        (setf (q+:geometry widget) (values x y (q+:width widget) (q+:height widget)))
        (setf maxheight (max maxheight (q+:height widget)))
        (incf x (q+:width widget))
        (when (< (q+:width flow-layout) (+ x (q+:width widget)))
          (incf y maxheight)
          (setf maxheight 0) (setf x 0)))
      (setf (q+:minimum-height flow-layout) (+ y maxheight)))))
