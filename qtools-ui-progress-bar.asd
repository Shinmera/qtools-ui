#|
 This file is a part of Qtools-UI
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-ui-progress-bar
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A progress bar"
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "progress-bar"))
  :depends-on (:qtools-ui-base))
