#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-ui-helpers
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A collection of useful little helper widgets."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "repaintable")
               (:file "mouse-propagator")
               (:file "draggable")
               (:file "selectable"))
  :depends-on (:qtools-ui-base
               :qtools-ui-layout))
