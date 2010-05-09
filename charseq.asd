(in-package :asdf)

(defsystem charseq
  :name "charseq"
  :version "0.1.1"
  :author "Takeru Ohta"
  :description "Provides a shareable, (simple-array character) typed string"
  :components ((:file "charseq")))