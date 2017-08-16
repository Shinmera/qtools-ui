#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-ui-container
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A basic superclass for arbitrary element container layouts."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "container"))
  :depends-on (:qtools-ui-base
               :qtools-ui-layout))
