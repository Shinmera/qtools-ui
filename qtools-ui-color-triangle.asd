#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-ui-color-triangle
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An HSV color wheel triangle as used in graphics applications."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "color-triangle"))
  :depends-on (:qtools-ui-base
               :qtgui
               :cl-opengl))
