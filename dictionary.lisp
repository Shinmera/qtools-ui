#|
 This file is a part of Qtools-UI
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Michał "phoe" Herda <phoe@disroot.org>
|#

(in-package #:org.shirakumo.qtools.ui)
(in-readtable :qtools)

(define-widget dictionary (qwidget)
  ((empty-browser-text :accessor empty-browser-text
                       :initarg :empty-browser-text)
   (not-found-text :accessor not-found-text
                   :initarg :not-found-text))
  (:default-initargs
    :empty-browser-text
    "<p align=center><i>Type your query below and hit Search.</i></p>"
    :not-found-text
    "<p align=center><i>The entry for \"~A\" was not found.</i></p>"))

(define-subwidget (dictionary browser)
    (make-instance 'dictionary-browser :dictionary dictionary)
  (setf (q+:html browser) empty-browser-text))

(define-subwidget (dictionary input) (q+:make-qlineedit))

(defun make-text-qtoolbutton (text)
  (let ((button (q+:make-qtoolbutton)))
    (setf (q+:text button) text
          (q+:tool-button-style button)
          (q+:qt.tool-button-text-only))
    button))

(define-subwidget (dictionary button) (make-text-qtoolbutton "Search")
  (setf (q+:focus input) (q+:qt.other-focus-reason)))

(define-subwidget (dictionary layout) (q+:make-qgridlayout dictionary)
  (setf (q+:contents-margins layout) (values 0 0 0 0))
  (q+:add-widget layout browser 0 0 1 2)
  (q+:add-widget layout input 1 0)
  (q+:add-widget layout button 1 1))

(defun trim-whitespace (string)
  (string-trim '(#\Space #\Newline #\Tab) string))

(define-slot (dictionary search) ()
  (declare (connected button (clicked)))
  (declare (connected input (return-pressed)))
  (let ((text (trim-whitespace (q+:text input))))
    (when (string/= "" text)
      (let ((response (htmlize-wordnet (wordnet:wordnet-describe* text))))
        (if (string/= "" response)
            (setf (q+:html browser) response)
            (setf (q+:html browser)
                  (format NIL not-found-text text))))))) ;; TODO parametrize text

(define-slot (dictionary set-focus-from-browser set-focus-from-browser)
             ((old "QWidget*") (new "QWidget*"))
  (declare (connected qt:*qapplication* (focus-changed "QWidget*" "QWidget*")))
  (%set-focus-from-browser dictionary old new))

(defmethod %set-focus-from-browser ((dictionary dictionary) old (new qobject))
  (with-slots-bound (dictionary dictionary)
    (when (eq new browser)
      (setf (q+:focus input) (q+:qt.other-focus-reason)))))

(defmethod %set-focus-from-browser ((dictionary dictionary) old new))

(defun htmlize-wordnet (results)
  (with-output-to-string (*standard-output*)
    (dolist (result results)
      (destructuring-bind
          (word-or-phrase part-of-speech glossaries synonyms antonyms) result
        (flet ((anchorize (x) (list (substitute #\_ #\Space x) x) ))
          (format T "<h2>~A (~A)</h2>~%" word-or-phrase
                  (string-downcase (string part-of-speech)))
          (format T "<ol>~%")
          (format T "~{  <li>~A</li>~%~}" glossaries)
          (format T "</ol>~%")
          (when synonyms
            (format T "<p>Synonyms: ~{~{~%  <a href=#~A>~A</a>~}~^,~}~%</p>~%"
                    (mapcar #'anchorize synonyms)))
          (when antonyms
            (format T "<p>Antonyms: ~{~{~%  <a href=#~A>~A</a>~}~^,~}~%</p>~%"
                    (mapcar #'anchorize antonyms))))))))

(define-widget dictionary-browser (qtextbrowser)
  ((clicked-anchor :accessor clicked-anchor :initform "")
   (dictionary :accessor dictionary :initarg :dictionary)))

(defmethod initialize-instance :after ((object dictionary-browser) &key)
  (let* ((palette (q+:palette object))
         (color (q+:color palette (q+:background-role object))))
    (when (and (< (q+:red color) 80)
               (< (q+:blue color) 80)
               (< (q+:green color) 80))
      (setf (q+:default-style-sheet (q+:document object))
            "a { color: #8888ff; }"))))

(define-override (dictionary-browser mouse-press-event) (event)
  (setf clicked-anchor
        (if (= (enum-value (q+:button event)) (q+:qt.left-button))
            (q+:anchor-at dictionary-browser (q+:pos event))
            NIL))
  (call-next-qmethod))

(define-override (dictionary-browser mouse-release-event) (event)
  (when (and (= (enum-value (q+:button event)) (q+:qt.left-button))
             (string/= clicked-anchor "")
             (string= clicked-anchor
                      (q+:anchor-at dictionary-browser (q+:pos event))))
    (let ((text (substitute #\Space #\_ (subseq clicked-anchor 1))))
      (setf (q+:text (slot-value dictionary 'input)) text
            (q+:html dictionary-browser)
            (htmlize-wordnet (wordnet:wordnet-describe* text)))))
  (call-next-qmethod))
