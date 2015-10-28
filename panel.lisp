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
  (setf (q+:cursor panel-titlebar) (q+:make-qcursor (q+:qt.open-hand-cursor)))
  (setf (q+:auto-fill-background panel-titlebar) T)
  (setf (q+:color (q+:palette panel-titlebar) (q+:qpalette.background))
        (q+:darker (q+:color (q+:palette panel-titlebar) (q+:qpalette.background)))))

(define-subwidget (panel-titlebar title) (q+:make-qlabel panel-titlebar)
  (setf (q+:style-sheet title) "padding: 0px 3px 0px 3px;")
  (when (title panel)
    (setf (q+:text title) (title panel))))

(define-subwidget (panel-titlebar attach-toggle) (q+:make-qpushbutton "Attach" panel-titlebar)
  (setf (q+:style-sheet attach-toggle) "padding: 0px 3px 0px 3px;")
  (setf (q+:flat attach-toggle) T)
  (setf (q+:cursor attach-toggle) (q+:make-qcursor (q+:qt.arrow-cursor)))
  (unless (detachable-p panel)
    (q+:hide attach-toggle)))

(define-subwidget (panel-titlebar collapse-toggle) (q+:make-qpushbutton "Collapse" panel-titlebar)
  (setf (q+:style-sheet collapse-toggle) "padding: 0px 3px 0px 3px;")
  (setf (q+:flat collapse-toggle) T)
  (setf (q+:cursor collapse-toggle) (q+:make-qcursor (q+:qt.arrow-cursor)))
  (unless (collapsable-p panel)
    (q+:hide collapse-toggle)))

(define-subwidget (panel-titlebar layout) (q+:make-qhboxlayout panel-titlebar)
  (setf (q+:alignment layout) (q+:qt.align-right))
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0)
  (q+:add-widget layout title)
  (q+:add-stretch layout 1)
  (q+:add-widget layout collapse-toggle)
  (q+:add-widget layout attach-toggle))

(define-slot (panel-titlebar attach-toggle) ()
  (declare (connected attach-toggle (pressed)))
  (setf (attached-p panel) (not (attached-p panel))))

(define-slot (panel-titlebar collapse-toggle) ()
  (declare (connected collapse-toggle (pressed)))
  (setf (collapsed-p panel) (not (collapsed-p panel))))

(defmethod drag-start ((panel-titlebar panel-titlebar) x y)
  (q+:qapplication-set-override-cursor (q+:make-qcursor (q+:qt.closed-hand-cursor))))

(defmethod drag-end ((panel-titlebar panel-titlebar) x y)
  (q+:qapplication-restore-override-cursor))

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
    (setf (q+:text attach-toggle) (if attached-p "Detach" "Attach"))))

(defmethod (setf detachable-p) (value (panel-titlebar panel-titlebar))
  (with-slots-bound (panel-titlebar panel-titlebar)
    (setf (q+:visible attach-toggle) value)))

(defmethod (setf collapsable-p) (value (panel-titlebar panel-titlebar))
  (with-slots-bound (panel-titlebar panel-titlebar)
    (setf (q+:visible collapse-toggle) value)))

(define-widget panel (QWidget compass)
  ((container :initarg :container :accessor panel-container)
   (title :initarg :title :accessor title)
   (attached-size :initform NIL :accessor attached-size)
   (detached-size :initform NIL :accessor detached-size)
   (taching :initform NIL :accessor taching)
   (detachable :initarg :detachable :accessor detachable-p)
   (collapsable :initarg :collapsable :accessor collapsable-p))
  (:default-initargs
    :container NIL
    :title NIL
    :detachable T
    :collapsable T))

(define-initializer (panel setup)
  (when (panel-container panel)
    (attach panel NIL))
  (setf (title panel) (title panel)))

(define-subwidget (panel titlebar) (make-instance 'panel-titlebar :panel panel)
  (setf (widget :north panel) titlebar))

(define-override (panel resize-event) (ev)
  (unless taching
    (cond ((attached-p panel)
           (fsetf (attached-size panel) (copy (q+:size panel))))
          (T
           (fsetf (detached-size panel) (copy (q+:geometry panel))))))
  (update panel)
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

(defmethod collapsed-p ((panel panel))
  (not (and (widget :center panel)
            (q+:is-visible (widget :center panel)))))

(defmethod (setf collapsed-p) (value (panel panel))
  (if value
      (collapse panel)
      (expand panel)))

(defmethod (setf detachable-p) :after (value (panel panel))
  (setf (detachable-p (slot-value panel 'titlebar)) value))

(defmethod (setf collapsable-p) :after (value (panel panel))
  (setf (attachable-p (slot-value panel 'titlebar)) value))

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
  (when (detachable-p panel)
    (remove-widget panel (panel-container panel))))

(defmethod expand ((panel panel))
  (when (widget :center panel)
    (q+:show (widget :center panel))
    (update panel)))

(defmethod collapse ((panel panel))
  (when (and (widget :center panel) (collapsable-p panel))
    (q+:hide (widget :center panel))
    (update panel)))

(defmethod drag ((panel panel) px py nx ny)
  (cond ((attached-p panel)
         (let* ((pos (q+:map-to-global panel (q+:make-qpoint nx ny)))
                (widget (q+:qapplication-widget-at pos)))
           (when (and (typep widget 'panel)
                      (eql (parent widget) (parent panel))
                      (not (eql widget panel)))
             (swap-widget widget panel (parent panel)))))
        (T
         (q+:move panel
                  (+ (q+:x panel) (- nx px))
                  (+ (q+:y panel) (- ny py))))))

(defmethod exit ((panel panel))
  (detach panel)
  (q+:close panel)
  (finalize panel))
