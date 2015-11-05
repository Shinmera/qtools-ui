#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-ui-listing
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A basic item listing widget that allows selection, sorting, and dragging."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "listing"))
  :depends-on (:qtools-ui-base
               :qtools-ui-container
               :qtools-ui-cell))
