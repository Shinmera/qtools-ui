#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-ui
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "layout")
               (:file "items")
               (:file "repaintable")
               (:file "mouse-propagator")
               (:file "draggable")
               (:file "selectable")
               (:file "cell")
               (:file "compass")
               (:file "container")
               (:file "splitter")
               (:file "panel")
               (:file "panel-container")
               (:file "keychord-editor")
               (:file "listing")
               (:file "slider")
               (:file "documentation"))
  :depends-on (:qtools
               :qtcore
               :qtgui))
