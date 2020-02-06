#|
 This file is a part of Qtools-UI
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Michał "phoe" Herda <phoe@disroot.org>

 This is a port of https://github.com/cameel/auto-resizing-text-edit/
 by Kamil Śliwak, released under the MIT license.
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget auto-resizing-textedit (qtextedit qui:fixed-qtextedit)
  ((minimum-lines :accessor minimum-lines :initarg :minimum-lines))
  (:default-initargs :minimum-lines 1))

(defmethod initialize-instance :after ((object auto-resizing-textedit) &key)
  (let ((size-policy (q+:size-policy object)))
    (setf (q+:height-for-width size-policy) T
          (q+:vertical-policy size-policy) (q+:qsizepolicy.preferred)
          (q+:size-policy object) size-policy))
  (setf (minimum-lines object) (minimum-lines object)))

(define-slot (auto-resizing-textedit update-geometry) ()
  (declare (connected auto-resizing-textedit (text-changed)))
  (q+:update-geometry auto-resizing-textedit))

(defmethod (setf minimum-lines) :after (nlines (object auto-resizing-textedit))
  (setf (q+:minimum-size object)
        (values (q+:width (q+:minimum-size object))
                (line-count-widget-height object nlines))))

(defun height-for-width (auto-resizing-textedit width)
  (let* ((margins (q+:contents-margins auto-resizing-textedit))
         (document-width (if (>= width (+ (q+:left margins) (q+:right margins)))
                             (- width (q+:left margins) (q+:right margins))
                             0))
         (document (q+:clone (q+:document auto-resizing-textedit))))
    (setf (q+:text-width document) document-width)
    (floor (+ (q+:top margins)
              (q+:height (q+:size document))
              (q+:bottom margins)))))

(define-override (auto-resizing-textedit height-for-width) (width)
  (height-for-width auto-resizing-textedit width))

(define-override (auto-resizing-textedit size-hint) ()
  (let* ((original-hint (call-next-qmethod))
         (width (q+:width original-hint)))
    (q+:make-qsize width (height-for-width auto-resizing-textedit width))))

(defun line-count-widget-height (auto-resizing-textedit nlines)
  (let* ((margins (q+:contents-margins auto-resizing-textedit))
         (document (q+:document auto-resizing-textedit))
         (document-margin (q+:document-margin document))
         (font-metrics (q+:make-qfontmetrics (q+:default-font document))))
    (floor (+ (q+:top margins)
              document-margin
              (* (max 1 nlines) (q+:height font-metrics))
              document-margin
              (q+:bottom margins)))))
