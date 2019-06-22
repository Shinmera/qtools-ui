#|
 This file is a part of Qtools-UI
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Michał "phoe" Herda <phoe@disroot.org>
|#


(asdf:defsystem qtools-ui-dictionary
  :license "zlib"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :maintainer "Michał \"phoe\" Herda <phoe@disroot.org>"
  :description "A widget allowing lookups in English WordNet dictionary."
  :homepage "https://Shinmera.github.io/qtools-ui/"
  :bug-tracker "https://github.com/Shinmera/qtools-ui/issues"
  :source-control (:git "https://github.com/Shinmera/qtools-ui.git")
  :serial T
  :components ((:file "dictionary"))
  :depends-on (:qtools-ui-base
               :qtools-ui-fixed-qtextedit
               :qtools-ui-helpers
               :wordnet))
