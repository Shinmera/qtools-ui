#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defun remove-newlines (string)
  (remove #\Linefeed string))

(defun set-foreground-color (widget color)
  (setf (q+:color (q+:palette widget) (q+:qpalette.foreground)) color))

(defun make-section-heading (parent format &rest args)
  (q+:make-qlabel (format NIL "<span style=\"font-weight: bold; font-size: 13pt;\">~?</span>"
                          format args) parent))

(defun invoke-gui-debugger (condition &optional (debugger-class 'debugger))
  (dissect:with-capped-stack ()
    (with-finalizing ((debugger (make-instance debugger-class :environment condition)))
      (q+:exec debugger))))

(define-widget debugger (QDialog)
  ((environment :initarg :condition :initarg :environment :accessor environment)))

(define-initializer (debugger setup 100)
  (setf environment
        (etypecase environment
          (dissect:environment environment)
          (condition (dissect:capture-environment environment))
          (null (dissect:capture-environment))))
  (setf (q+:window-title debugger) (format NIL "Debugger~@[ [~a]~]" (type-of (dissect:environment-condition environment)))))

(define-subwidget (debugger condition)
    (if (dissect:environment-condition environment)
        (make-instance 'condition-view :condition (dissect:environment-condition environment)
                                       :debugger debugger)
        (q+:make-qwidget debugger)))

(define-subwidget (debugger restarts)
    (make-instance 'restart-view :restarts (dissect:environment-restarts environment)
                                 :debugger debugger))

(define-subwidget (debugger stacktrace)
    (make-instance 'stacktrace-view :stacktrace (dissect:environment-stack environment)
                                    :debugger debugger))

(define-subwidget (debugger scroller) (q+:make-qscrollarea debugger)
  (setf (q+:widget scroller) stacktrace)
  (setf (q+:widget-resizable scroller) T)
  (setf (q+:vertical-scroll-bar-policy scroller) (q+:qt.scroll-bar-always-on))
  (setf (q+:horizontal-scroll-bar-policy scroller) (q+:qt.scroll-bar-always-off)))

(define-subwidget (debugger layout) (q+:make-qvboxlayout debugger)
  (q+:add-widget layout condition)
  (q+:add-widget layout (make-section-heading debugger "Active Restarts:"))
  (q+:add-widget layout restarts)
  (q+:add-widget layout (make-section-heading debugger "Stack Trace:"))
  (q+:add-widget layout scroller))

(defmethod exit-with-restart ((debugger debugger) (restart dissect:restart))
  (q+:close debugger)
  (dissect:invoke restart))

(defmethod exit-with-restart ((debugger debugger) (restart symbol))
  (q+:close debugger)
  (invoke-restart restart))

(define-override (debugger key-release-event) (ev)
  (flet ((exit-with-nth (n)
           (let ((restart (nth n (dissect:environment-restarts environment))))
             (when restart (exit-with-restart debugger restart)))))
    (qtenumcase (q+:key ev)
      ((q+:qt.key_q) (q+:close debugger))
      ((q+:qt.key_c) (exit-with-restart debugger 'continue))
      ((q+:qt.key_a) (exit-with-restart debugger 'abort))
      ((q+:qt.key_0) (exit-with-nth 0))
      ((q+:qt.key_1) (exit-with-nth 1))
      ((q+:qt.key_2) (exit-with-nth 2))
      ((q+:qt.key_3) (exit-with-nth 3))
      ((q+:qt.key_4) (exit-with-nth 4))
      ((q+:qt.key_5) (exit-with-nth 5))
      ((q+:qt.key_6) (exit-with-nth 6))
      ((q+:qt.key_7) (exit-with-nth 7))
      ((q+:qt.key_8) (exit-with-nth 8))
      ((q+:qt.key_8) (exit-with-nth 9))))
  (stop-overriding))

(define-widget condition-view (QWidget)
  ((condition :initarg :condition)
   (debugger :initarg :debugger)))

(define-subwidget (condition-view report) (q+:make-qlabel condition-view)
  (setf (q+:word-wrap report) T)
  (setf (q+:text report) (format NIL "~a" condition)))

(define-subwidget (condition-view type)
    (make-section-heading condition-view "Condition of type ~a" (type-of condition))
  (set-foreground-color type (q+:make-qcolor 250 50 50)))

(define-subwidget (condition-view layout) (q+:make-qvboxlayout condition-view)
  (setf (q+:margin layout) 0)
  (q+:add-widget layout type)
  (q+:add-widget layout report))

(define-widget restart-view (QWidget)
  ((restarts :initarg :restarts)
   (debugger :initarg :debugger)))

(define-subwidget (restart-view layout) (q+:make-qvboxlayout restart-view)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0)
  (dolist (restart restarts)
    (q+:add-widget layout (make-instance 'restart-item :restart restart :debugger debugger))))

(define-widget restart-item (QWidget)
  ((restart :initarg :restart)
   (debugger :initarg :debugger)))

(define-subwidget (restart-item name) (q+:make-qpushbutton (princ-to-string (dissect:name restart)) restart-item)
  (setf (q+:size-policy name) (values (q+:qsizepolicy.maximum) (q+:qsizepolicy.maximum))))

(define-subwidget (restart-item report) (q+:make-qlabel restart-item)
  (setf (q+:text report) (remove-newlines (dissect:report restart))))

(define-subwidget (restart-item layout) (q+:make-qhboxlayout restart-item)
  (setf (q+:margin layout) 0)
  (q+:add-widget layout name)
  (q+:add-widget layout report))

(define-slot (restart-item invoke) ()
  (declare (connected name (clicked)))
  (exit-with-restart debugger restart))

(define-widget stacktrace-view (QWidget)
  ((stacktrace :initarg :stacktrace)
   (debugger :initarg :debugger)))

(define-subwidget (stacktrace-view layout) (q+:make-qformlayout stacktrace-view)
  (set-foreground-color stacktrace-view (q+:make-qcolor 120 120 120))
  (setf (q+:margin layout) 0)
  ;; (setf (q+:spacing layout) 0)
  (dolist (frame stacktrace)
    (q+:add-row layout (format NIL "~3d" (dissect:pos frame))
                (make-instance 'call-item :frame frame :debugger debugger))))

(define-widget call-item (QWidget)
  ((frame :initarg :frame)
   (debugger :initarg :debugger)))

(define-subwidget (call-item call) (q+:make-qlabel call-item)
  (set-foreground-color call (q+:make-qcolor 220 220 220))
  (setf (q+:text call) (remove-newlines (prin1-to-string (dissect:call frame))))
  (setf (q+:size-policy call) (values (q+:qsizepolicy.expanding) (q+:qsizepolicy.maximum))))

(define-subwidget (call-item args) (make-instance 'arglist-view :args (dissect:args frame) :debugger debugger)
  (setf (q+:visible args) NIL))

(define-subwidget (call-item layout) (q+:make-qvboxlayout call-item)
  (setf (q+:margin layout) 0)
  (q+:add-widget layout call)
  (q+:add-widget layout args))

(define-override (call-item mouse-release-event) (ev)
  (setf (q+:visible args) (not (q+:is-visible args)))
  (stop-overriding))

(define-widget arglist-view (QWidget)
  ((args :initarg :args)
   (debugger :initarg :debugger)))

(define-subwidget (arglist-view layout) (q+:make-qformlayout arglist-view)
  (setf (q+:margin layout) 0)
  (loop for arg in args
        for i from 0
        do (q+:add-row layout
                       (format NIL "~3d" i)
                       (make-instance 'arg-item :arg arg :debugger debugger))))

(define-widget arg-item (QLabel)
  ((arg :initarg :arg)
   (debugger :initarg :debugger)))

(define-initializer (arg-item setup)
  (set-foreground-color arg-item (q+:make-qcolor 230 0 0))
  (setf (q+:word-wrap arg-item) T)
  (setf (q+:text arg-item) (prin1-to-string arg)))
