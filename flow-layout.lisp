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
    (loop for maxheight = 0 then (max maxheight (q+:height widget))
          for x = 0 then (+ x (q+:width widget))
          for widget in (widgets flow-layout)
          for y = 0 then (if (< (q+:width flow-layout) (+ x (q+:width widget)))
                             (+ y (prog1 maxheight (setf maxheight 0) (setf x 0)))
                             y)
          do (setf (q+:geometry widget) (values x y (q+:width widget) (q+:height widget)))
          finally (setf (q+:minimum-height flow-layout) (+ y maxheight)))))
