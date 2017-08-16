#|
 This file is a part of Qtools-UI
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-ui-executable
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Provides synchronisation between threads by allowing execution of functions within the gui thread."
  :homepage "https://github.com/Shinmera/qtools-ui"
  :serial T
  :components ((:file "executable"))
  :depends-on (:qtools-ui-base
               :bordeaux-threads))
