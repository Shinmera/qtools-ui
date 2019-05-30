#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Michał "phoe" Herda <phoe@disroot.org>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget placeholder-text-edit (qtextedit fixed-qtextedit)
  ((placeholder :accessor placeholder :initarg :placeholder)
   (placeholder-font :accessor placeholder-font :initarg :placeholder-font))
  (:default-initargs
   :placeholder NIL
   :placeholder-font NIL))

(define-override (placeholder-text-edit paint-event) (ev)
  (when (and (q+:is-empty (q+:document placeholder-text-edit)) placeholder)
    (let ((viewport (q+:viewport placeholder-text-edit)))
      (with-finalizing ((painter (q+:make-qpainter viewport))
                        (rect (q+:rect placeholder-text-edit)))
        (let* ((color (q+:color (q+:brush (q+:pen painter))))
               (document (q+:document placeholder-text-edit))
               (margin (truncate (q+:document-margin document)))
               (current-font (q+:current-font placeholder-text-edit)))
          (setf (q+:font painter) (or placeholder-font current-font)
                (q+:alpha color) 127)
          (q+:adjust rect margin margin (- margin) (- margin))
          (q+:draw-text painter rect (q+:qt.text-word-wrap)
                        (placeholder placeholder-text-edit))
          (setf (q+:alpha color) 255)))))
  (call-next-qmethod))
