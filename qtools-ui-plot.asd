#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-ui-plot
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A one dimensional plotter"
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "plot"))
  :depends-on (:qtools-ui-base
               :qtools-ui-helpers))
