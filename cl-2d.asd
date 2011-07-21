(defpackage #:cl-2d-asd
  (:use :cl :asdf))

(in-package :cl-2d-asd)

(defsystem :cl-2d
  :description "2D graphs for Common Lisp, using Cairo"
  :version "0.1"
  :author "Tamas K Papp"
  :license "LLGPL"
  :serial t
  :components ((:file "package")
	       (:file "utilities")
	       (:file "contexts")
	       (:file "frame")
	       (:file "mapping")
	       (:file "drawing-area")
	       (:file "styles")
	       (:file "axis")
	       (:file "color-mapping")
	       (:file "legend")
	       (:file "symbols")
	       (:file "plot"))
  :depends-on (:cl-cairo2
	       :iterate
	       :cl-colors
	       :alexandria
	       :metabang-bind
	       :cl-num-utils
               :xarray))
