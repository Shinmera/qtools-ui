#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defun remove-newlines (string)
  (remove #\Linefeed string))

(defun invoke-gui-debugger (condition)
  (dissect:with-capped-stack ()
    (with-finalizing ((debugger (make-instance 'debugger :environment condition)))
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
  (q+:add-widget layout (q+:make-qlabel "<b>Active Restarts:</b>" debugger))
  (q+:add-widget layout restarts)
  (q+:add-widget layout (q+:make-qlabel "<b>Stack Trace:</b>" debugger))
  (q+:add-widget layout scroller))

(define-widget condition-view (QWidget)
  ((condition :initarg :condition)
   (debugger :initarg :debugger)))

(define-subwidget (condition-view report) (q+:make-qlabel condition-view)
  (setf (q+:word-wrap report) T)
  (setf (q+:text report) (format NIL "~a" condition)))

(define-subwidget (condition-view type) (q+:make-qlabel condition-view)
  (setf (q+:color (q+:palette type) (q+:qpalette.foreground)) (q+:make-qcolor 250 50 50))
  (setf (q+:text type) (format NIL "Condition of type ~a" (type-of condition))))

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
  (q+:close debugger)
  (funcall (dissect:restart restart)))

(define-widget stacktrace-view (QWidget)
  ((stacktrace :initarg :stacktrace)
   (debugger :initarg :debugger)))

(define-subwidget (stacktrace-view layout) (q+:make-qformlayout stacktrace-view)
  (setf (q+:color (q+:palette stacktrace-view) (q+:qpalette.foreground)) (q+:make-qcolor 120 120 120))
  (setf (q+:margin layout) 0)
  ;; (setf (q+:spacing layout) 0)
  (dolist (frame stacktrace)
    (q+:add-row layout (format NIL "~3d" (dissect:pos frame))
                (make-instance 'call-item :frame frame :debugger debugger))))

(define-widget call-item (QWidget)
  ((frame :initarg :frame)
   (debugger :initarg :debugger)))

(define-subwidget (call-item call) (q+:make-qlabel call-item)
  (setf (q+:color (q+:palette call) (q+:qpalette.foreground)) (q+:make-qcolor 220 220 220))
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
  (setf (q+:color (q+:palette arg-item) (q+:qpalette.foreground)) (q+:make-qcolor 230 0 0))
  (setf (q+:text arg-item) (prin1-to-string arg)))
