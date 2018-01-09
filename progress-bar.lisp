#|
 This file is a part of Qtools-UI
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget progress-bar (QWidget)
  ((text :initarg :text :accessor text)
   (timeout :initform 0 :accessor timeout)
   (progress :initarg :progress :accessor progress)
   (minimum :initarg :minimum :accessor minimum)
   (maximum :initarg :maximum :accessor maximum))
  (:default-initargs
    :text "Working..."
    :progress NIL
    :minimum 0
    :maximum 100))

(defmethod initialize-instance :after ((bar progress-bar) &key progress)
  (when progress
    (setf (progress bar) (progress bar))))

(defmethod (setf progress) :around (value (bar progress-bar))
  (etypecase value
    (real
     (let* ((max (maximum bar))
            (min (minimum bar))
            (value (max (min value max) min)))
       (call-next-method value bar)
       (setf (text bar) (format NIL "~d%" (round (/ (- value min) max 1/100))))))
    (null
     (call-next-method))))

(define-subwidget (progress-bar timer) (q+:make-qtimer progress-bar)
  (q+:start timer (round 1000/30)))

(define-slot (progress-bar update-timeout) ()
  (declare (connected timer (timeout)))
  (let ((w (q+:width progress-bar)))
    (setf (timeout progress-bar) (- (mod (+ (timeout progress-bar) w (round (/ w 30)))
                                         (* 2 w))
                                    w)))
  (q+:update progress-bar))

(define-override (progress-bar paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter progress-bar))
                    (rect (q+:rect progress-bar)))
    (let ((max (maximum progress-bar))
          (min (minimum progress-bar))
          (progress (progress progress-bar)))
      (cond (progress
             (setf (q+:width rect) (round (* (q+:width rect) (/ (- progress min) max)))))
            (T
             (setf (q+:left rect) (- (abs (timeout progress-bar)) 50))
             (setf (q+:right rect) (+ (abs (timeout progress-bar)) 50))))
      (q+:fill-rect painter rect (q+:highlight (q+:palette progress-bar)))
      (q+:draw-text painter (q+:rect progress-bar) (q+:qt.align-center) (text progress-bar)))))
