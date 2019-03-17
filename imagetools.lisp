#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Michał "phoe" Herda <phoe@disroot.org>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defun hue-shift (image ratio)
  (let* ((ratio (mod ratio 1))
         (percentage (round (* (sqrt ratio) 207)))
         (channel (truncate (* (sqrt ratio) 255))))
    (with-finalizing ((mod-image (q+:make-qimage 1 1 (q+:qimage.format_rgb32)))
                      (color (q+:make-qcolor channel 0 0)))
      (q+:fill mod-image color)
      (q+:blitz-modulate image mod-image nil (q+:blitz.hue-shift)
                         percentage (q+:blitz.grayscale)))))
