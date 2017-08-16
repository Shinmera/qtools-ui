#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-ui-compass
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A compass layout orienting things N/E/S/W/C."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "compass"))
  :depends-on (:qtools-ui-base
               :qtools-ui-layout))
