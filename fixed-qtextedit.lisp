#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Michał "phoe" Herda <phoe@disroot.org>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget fixed-qtextedit (qtextedit) ())

(define-subwidget (fixed-qtextedit fix-context-menu-widget) (q+:make-qwidget))

(define-override (fixed-qtextedit context-menu-event) (event)
  ;; I have no idea why it works this way, but it seems to work.
  (call-next-qmethod)
  (let ((position (q+:pos event)))
    ;; Fix cursor position after the context menu disappears.
    (let* ((cursor (q+:cursor-for-position fixed-qtextedit position)))
      (setf (q+:text-cursor fixed-qtextedit) cursor))
    ;; Display the context menu.
    (with-finalizing ((menu (q+:create-standard-context-menu fixed-qtextedit)))
      (q+:exec menu (q+:global-pos event)))
    ;; Work around the bug.
    (q+:show fix-context-menu-widget)
    (setf (q+:focus fix-context-menu-widget) 0)
    (q+:hide fix-context-menu-widget)))
