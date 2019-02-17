#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Michał "phoe" Herda <phoe@disroot.org>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defun svg-pixmap (pathname &key (scale 1) (renderer nil rendererp))
  (flet ((%svg-pixmap (svg-namestring scale renderer)
           (q+:load renderer svg-namestring)
           (let* ((size (q+:default-size renderer))
                  (x (q+:width size))
                  (y (q+:height size))
                  (pixmap (q+:make-qpixmap (round (* scale x))
                                           (round (* scale y)))))
             (with-finalizing ((color (q+:make-qcolor 0 0 0 0)))
               (q+:fill pixmap color))
             (with-finalizing ((painter (q+:make-qpainter pixmap)))
               (q+:render renderer painter (q+:make-qrectf (q+:rect pixmap))))
             pixmap)))
    (let ((namestring (uiop:native-namestring pathname)))
      (if rendererp
          (%svg-pixmap namestring scale renderer)
          (with-finalizing ((renderer (q+:make-qsvgrenderer)))
            (%svg-pixmap namestring scale renderer))))))
