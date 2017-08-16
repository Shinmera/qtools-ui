#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-ui-debugger
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A portable debugger to handle and display errors."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "debugger"))
  :depends-on (:qtools-ui-base
               :dissect))
