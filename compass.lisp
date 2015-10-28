#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget compass (QWidget layout)
  ((north :initarg :north)
   (east :initarg :east)
   (south :initarg :south)
   (west :initarg :west)
   (center :initarg :center))
  (:default-initargs
    :north NIL
    :east NIL
    :south NIL
    :west NIL
    :center NIL))

(defmethod widget ((place symbol) (compass compass))
  (with-slots-bound (compass compass)
    (ecase place
      (:north north)
      (:east east)
      (:south south)
      (:west west)
      (:center center))))

(defmethod (setf widget) ((widget qobject) (place symbol) (compass compass))
  (with-slots-bound (compass compass)
    (macrolet ((setplace (symb to)
                 `(progn (when ,symb (setf (parent ,symb) NIL))
                         (setf ,symb ,to))))
      (ecase place
        (:north (setplace north widget))
        (:east (setplace east widget))
        (:south (setplace south widget))
        (:west (setplace west widget))
        (:center (setplace center widget)))))
  (when widget
    (setf (parent widget) compass))
  (update compass))

(defmethod (setf widget) ((widget qobject) (place qobject) (compass compass))
  (setf (widget (or (widget-position place compass)
                    (error "~a is not contained in ~a." widget compass))
                compass)
        widget))

(defmethod widget-position (widget (compass compass) &key key test test-not)
  (with-slots-bound (compass compass)
    (flet ((compare (field)
             (cond (test-not (not (funcall test-not (funcall key field) widget)))
                   (test (funcall test (funcall key field) widget)))))
      (cond ((compare north) :north)
            ((compare east) :east)
            ((compare south) :south)
            ((compare west) :west)
            ((compare center) :center)))))

(defmethod find-widget (widget (compass compass) &key key test test-not)
  (with-slots-bound (compass compass)
    (flet ((compare (field)
             (cond (test-not (not (funcall test-not (funcall key field) widget)))
                   (test (funcall test (funcall key field) widget)))))
      (cond ((compare north) north)
            ((compare east) east)
            ((compare south) south)
            ((compare west) west)
            ((compare center) center)))))

(defmethod add-widget ((widget qobject) (compass compass))
  (setf (widget :center compass) widget))

(defmethod insert-widget ((widget qobject) (place symbol) (compass compass))
  (setf (widget place compass) widget))

(defmethod insert-widget ((widget qobject) (place qobject) (compass compass))
  (setf (widget (or (widget-position place compass)
                    (error "~a is not contained in ~a." place compass))
                compass)
        widget))

(defmethod remove-widget ((place symbol) (compass compass))
  (prog1 (widget place compass)
    (setf (widget place compass) NIL)))

(defmethod remove-widget ((widget qobject) (compass compass))
  (remove-widget (or (widget-position widget compass)
                     (error "~a is not contained in ~a." widget compass))
                 compass))

(defmethod swap-widgets (a b (compass compass))
  (with-slots-bound (compass compass)
    (let ((a (or (widget-position a compass)
                 (error "~a is not contained in ~a." a compass)))
          (b (or (widget-position b compass)
                 (error "~a is not contained in ~a." b compass))))
      (let ((wa (widget a compass))
            (wb (widget b compass)))
        (flet ((set-place (place widget)
                 (ecase place
                   (:north (setf north widget))
                   (:east (setf east widget))
                   (:south (setf south widget))
                   (:west (setf west widget))
                   (:center (setf center widget)))))
          (set-place a wa)
          (set-place b wb))))))

(define-initializer (compass setup) ()
  (when north (setf (parent north) compass))
  (when east (setf (parent east) compass))
  (when south (setf (parent south) compass))
  (when west (setf (parent west) compass))
  (when center (setf (parent center) compass))
  (update compass))

(defmethod update ((compass compass))
  (with-slots-bound (compass compass)
    (macrolet ((mv (form)
                 `(if ,(third form) ,form 0))
               (set-geometry (target x y w h)
                 `(when ,target (setf (q+:geometry ,target) (values ,x ,y ,w ,h)))))
      (let ((ns (when north (q+:size-hint north)))
            (es (when east (q+:size-hint east)))
            (ss (when south (q+:size-hint south)))
            (ws (when west (q+:size-hint west))))
        (set-geometry north
                      0 0
                      (q+:width compass) (q+:height ns))
        (set-geometry south
                      0 (- (q+:height compass) (q+:height north))
                      (q+:width compass) (q+:height ss))
        (set-geometry west
                      0 (mv (q+:height north))
                      (q+:width ws) (- (q+:height compass) (mv (q+:height north)) (mv (q+:height south))))
        (set-geometry east
                      (- (q+:width compass) (q+:width east)) (mv (q+:height north))
                      (q+:width es) (- (q+:height compass) (mv (q+:height north)) (mv (q+:height south))))
        (set-geometry center
                      (mv (q+:width west))
                      (mv (q+:height north))
                      (- (q+:width compass) (mv (q+:width west)) (mv (q+:width east)))
                      (- (q+:height compass) (mv (q+:height north)) (mv (q+:height south)))))
      (setf (q+:minimum-width compass)
            (max (+ (mv (q+:width west)) (mv (q+:minimum-width center)) (mv (q+:width east)))
                 (mv (q+:minimum-width north))
                 (mv (q+:minimum-width south))))
      (setf (q+:minimum-height compass)
            (max (+ (mv (q+:height north)) (mv (q+:minimum-height center)) (mv (q+:height south)))
                 (mv (q+:minimum-height west))
                 (mv (q+:minimum-height east)))))))
