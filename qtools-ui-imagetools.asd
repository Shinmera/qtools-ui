#|
 This file is a part of Qtools-UI
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Michał "phoe" Herda <phoe@disroot.org>
|#

(asdf:defsystem qtools-ui-imagetools
  :license "zlib"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :maintainer "Michał \"phoe\" Herda <phoe@disroot.org>"
  :description "Toolkit for dealing with QImage files in Qt."
  :homepage "https://Shinmera.github.io/qtools-ui/"
  :bug-tracker "https://github.com/Shinmera/qtools-ui/issues"
  :source-control (:git "https://github.com/Shinmera/qtools-ui.git")
  :serial T
  :components ((:file "imagetools"))
  :depends-on (:qtools-ui-base
               :qimageblitz))
