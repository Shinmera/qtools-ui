#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-ui-color-picker
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A color picker dialog alternative to QColorChooser"
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "color-picker"))
  :depends-on (:qtools-ui-base
               :qtools-ui-helpers
               :qtools-ui-dialog
               :qtools-ui-color-triangle
               :qtools-ui-color-history
               :qtools-ui-color-sliders))
