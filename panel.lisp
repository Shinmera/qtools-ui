#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(defgeneric attached-p (panel))
(defgeneric attach (panel container))
(defgeneric detach (panel))
(defgeneric panel-container (panel))
(defgeneric exit (panel))

(define-widget panel-titlebar (QWidget draggable)
  ((panel :initarg :panel :accessor panel))
  (:default-initargs
    :panel (error "PANEL required.")))

(define-initializer (panel-titlebar setup)
  (setf (q+:auto-fill-background panel-titlebar) T)
  (setf (q+:color (q+:palette panel-titlebar) (q+:qpalette.background))
        (q+:darker (q+:color (q+:palette panel-titlebar) (q+:qpalette.background)))))

(define-subwidget (panel-titlebar title) (q+:make-qlabel panel-titlebar)
  (setf (q+:style-sheet title) "padding: 0px 3px 0px 3px;")
  (when (title panel)
    (setf (q+:text title) (title panel))))

(define-subwidget (panel-titlebar attach) (q+:make-qpushbutton "Attach" panel-titlebar)
  (setf (q+:style-sheet attach) "padding: 0px 3px 0px 3px;")
  (setf (q+:flat attach) T))

(define-subwidget (panel-titlebar detach) (q+:make-qpushbutton "Detach" panel-titlebar)
  (setf (q+:style-sheet detach) "padding: 0px 3px 0px 3px;")
  (setf (q+:flat detach) T)
  (q+:hide detach))

(define-subwidget (panel-titlebar layout) (q+:make-qhboxlayout panel-titlebar)
  (setf (q+:alignment layout) (q+:qt.align-right))
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0)
  (q+:add-widget layout title)
  (q+:add-stretch layout 1)
  (q+:add-widget layout attach)
  (q+:add-widget layout detach))

(define-slot (panel-titlebar attach) ()
  (declare (connected attach (pressed)))
  (attach panel NIL))

(define-slot (panel-titlebar detach) ()
  (declare (connected detach (pressed)))
  (detach panel))

(defmethod drag ((panel-titlebar panel-titlebar) px py nx ny)
  (drag (panel panel-titlebar) px py nx ny))

(defmethod title ((panel-titlebar panel-titlebar))
  (with-slots-bound (panel-titlebar panel-titlebar)
    (q+:text title)))

(defmethod (setf title) (value (panel-titlebar panel-titlebar))
  (with-slots-bound (panel-titlebar panel-titlebar)
    (setf (q+:text title) (or value ""))))

(defmethod attached-p ((panel-titlebar panel-titlebar))
  (attached-p (panel panel-titlebar)))

(defmethod (setf attached-p) (attached-p (panel-titlebar panel-titlebar))
  (with-slots-bound (panel-titlebar panel-titlebar)
    (setf (q+:visible attach) (not attached-p))
    (setf (q+:visible detach) attached-p)))

(define-widget panel (QWidget)
  ((container :initarg :container :accessor panel-container)
   (title :initarg :title :accessor title)
   (attached-size :initform NIL :accessor attached-size)
   (detached-size :initform NIL :accessor detached-size)
   (taching :initform NIL :accessor taching))
  (:default-initargs
    :container NIL
    :title NIL))

(define-initializer (panel setup)
  (when (panel-container panel)
    (attach panel (panel-container panel)))
  (setf (title panel) (title panel))
  (setf (q+:minimum-size panel) (values 100 50)))

(define-subwidget (panel titlebar) (make-instance 'panel-titlebar :panel panel))

(define-subwidget (panel layout) (q+:make-qvboxlayout panel)
  (setf (q+:alignment layout) (q+:qt.align-top))
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0)
  (q+:add-widget layout titlebar))

(define-override (panel resize-event) (ev)
  (unless taching
    (cond ((attached-p panel)
           (fsetf (attached-size panel) (copy (q+:size panel))))
          (T
           (fsetf (detached-size panel) (copy (q+:geometry panel))))))
  (stop-overriding))

(define-override (panel move-event) (ev)
  (unless taching
    (when (not (attached-p panel))
      (fsetf (detached-size panel) (copy (q+:geometry panel)))))
  (stop-overriding))

(defmethod attached-p ((panel panel))
  (not (null (parent panel))))

(defmethod (setf attached-p) (value (panel panel))
  (if value
      (attach panel NIL)
      (detach panel)))

(defmethod (setf title) :after (title (panel panel))
  (with-slots-bound (panel panel)
    (let ((title (or title "")))
      (setf (q+:window-title panel) title)
      (setf (title titlebar) title))))

(defmethod add-widget ((panel panel) new-container)
  (with-slots-bound (panel panel)
    (when (attached-p panel)
      (error "~a is already attached to ~a" panel container))
    (unless new-container
      (error "~a cannot be attached to nothing." panel))
    (setf taching T)
    (setf container new-container)
    (setf (q+:window-flags panel) (q+:qt.widget))
    (setf (attached-p titlebar) T)
    (when attached-size
      (q+:resize panel attached-size))
    (call-next-method)
    (setf taching NIL)))

(defmethod remove-widget ((panel panel) old-container)
  (with-slots-bound (panel panel)
    (unless (attached-p panel)
      (error "~a is not attached to anything!" panel))
    (unless (eql old-container container)
      (error "~a is not attached to ~a." panel old-container))
    (setf taching T)
    (call-next-method)
    (setf (q+:window-flags panel)
          (logior (q+:qt.window-stays-on-top-hint)
                  (q+:qt.tool)
                  (q+:qt.frameless-window-hint)))
    (q+:show panel)
    (setf (attached-p titlebar) NIL)
    (q+:activate-window panel)
    (when detached-size
      (setf (q+:geometry panel) detached-size))
    (setf taching NIL)))

(defmethod attach ((panel panel) (container null))
  (attach panel (panel-container panel)))

(defmethod attach ((panel panel) new-container)
  (add-widget panel new-container))

(defmethod detach ((panel panel))
  (remove-widget panel (panel-container panel)))

(defmethod drag ((panel panel) px py nx ny)
  (cond ((attached-p panel)
         (let* ((pos (q+:map-to-global panel (q+:make-qpoint nx ny)))
                (widget (q+:qapplication-widget-at pos)))
           (when (and (typep widget 'panel)
                      (eql (parent widget) (parent panel))
                      (not (eql widget panel)))
             (v:info :test "? ~a ~a" widget panel)
             (swap-widget widget panel (parent panel)))))
        (T
         (q+:move panel
                  (+ (q+:x panel) (- nx px))
                  (+ (q+:y panel) (- ny py))))))

(defmethod exit ((panel panel))
  (detach panel)
  (q+:close panel)
  (finalize panel))
