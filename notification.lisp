#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget notification (QWidget)
  ())

(defmethod initialize-instance :after ((notification notification) &key (title "Notification") message (timeout 2000)
                                                                        (width 350) (height 150))
  (setf (q+:text (slot-value notification 'title)) title)
  (setf (q+:html (slot-value notification 'message)) message)
  (when timeout (q+:start (slot-value notification 'timer) timeout))
  (setf (q+:window-flags notification) (logior (q+:qt.dialog)
                                               (q+:qt.frameless-window-hint)
                                               (q+:qt.window-stays-on-top-hint)))
  (setf (q+:attribute notification) (q+:qt.wa_show-without-activating))
  (q+:resize notification width height)
  (let* ((desktop (q+:qapplication-desktop))
         (geometry (q+:screen-geometry desktop (q+:primary-screen desktop))))
    (q+:translate geometry (- width) (- height))
    (q+:move notification (q+:bottom-right geometry))))

(define-subwidget (notification title)
    (q+:make-qlabel "Notification"))

(define-subwidget (notification close)
    (q+:make-qpushbutton "x")
  (setf (q+:fixed-width close) 20)
  (setf (q+:flat close) T)
  (connect! close (clicked) notification (close)))

(define-subwidget (notification message)
    (q+:make-qtextedit)
  (setf (q+:read-only message) T))

(define-subwidget (notification layout)
    (q+:make-qgridlayout notification)
  (setf (q+:margin layout) 3)
  (setf (q+:spacing layout) 0)
  (q+:add-widget layout title 0 0 1 1)
  (q+:add-widget layout close 0 1 1 1)
  (q+:add-widget layout message 1 0 1 2))

(define-subwidget (notification timer)
    (q+:make-qtimer notification)
  (setf (q+:single-shot timer) T))

(define-slot (notification close-timer) ()
  (declare (connected timer (timeout)))
  (if (q+:is-active-window notification)
      (q+:start timer 2000)
      (q+:close notification)))
