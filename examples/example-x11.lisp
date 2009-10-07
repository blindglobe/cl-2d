(in-package :cl-user)
(require :cl-2d)
(in-package :cl-2d)
(require :cl-cairo2)
(require :cl-cairo2-x11)
(use-package '(:cl-cairo2 :cl-2d :cl-numlib :cl-colors :bind))

;;; this is how you create an X11 frame.  If you supply a
;;; background-color to as-frame, each plot will clear the frame with
;;; this color.

(defparameter *frame* (as-frame (create-xlib-image-context 800 600)
			       :background-color +white+))


(plot-function *frame* #'exp (interval-of 0 2) :x-title "x" :y-title "exp(x)")

;;; split the frame, and you can draw on the subframes independently.
;;; I do this a lot.

(bind ((#2A((f1 f2) (f3 f4)) (split-frame *frame* (percent 50) (percent 50))))
  (defparameter *f1* f1)
  (defparameter *f2* f2)
  (defparameter *f3* f3)
  (defparameter *f4* f4))

(clear *frame*)
(plot-function *f1* #'sin (interval-of 0 2) :x-title "x" :y-title "sin(x)")
(plot-function *f2* #'cos (interval-of 0 2) :x-title "x" :y-title "cos(x)")
(plot-function *f3* #'tan (interval-of 0 2) :x-title "x" :y-title "tan(x)")
(plot-function *f4* #'/ (interval-of 0 2) :x-title "x" :y-title "1/x")
