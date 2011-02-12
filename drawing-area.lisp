(in-package :cl-2d)

;;;;  drawing-area
;;;;
;;;;  A drawing area is a frame with two mappings on the x and y axis.
;;;;  When created with setup-drawing-area, snapping to pixels and
;;;;  half-pixels is automatically set up when needed.

(defclass drawing-area (frame)
  ((x-mapping :initarg :x-mapping :reader x-mapping)
   (y-mapping :initarg :y-mapping :reader y-mapping)))

(defun setup-drawing-area (frame x-interval y-interval &key
			   (x-mapping-type 'linear-mapping)
			   (y-mapping-type 'linear-mapping))
  "Setup and return a drawing area [x-lower,x-upper] x
[y-lower,y-upper] which maps to frame.  Mappings snap to pixels
conditional on whether the context is pixel based."
  (with-slots ((h-int horizontal-interval)
	       (v-int vertical-interval)
	       context background-color) frame
    (flet ((maybe-constant-mapping (interval mapping-type)
             (if (zerop (interval-width interval))
                 'constant-mapping
                 mapping-type)))
      (let ((snap-p (pixel-based-p context)))
        (make-instance 'drawing-area 
                       :horizontal-interval h-int
                       :vertical-interval v-int :context context
                       :background-color background-color
                       :x-mapping (make-instance (maybe-constant-mapping 
                                                  x-interval x-mapping-type)
                                                  :domain x-interval
                                                  :range h-int
                                                  :snap-p snap-p)
                       :y-mapping (make-instance (maybe-constant-mapping
                                                  y-interval y-mapping-type)
                                                 :domain y-interval
                                                 :range v-int
                                                 :snap-p snap-p))))))

(defun x-domain (drawing-area)
  "Return the X domain of a drawing area."
  (domain (x-mapping drawing-area)))

(defun y-domain (drawing-area)
  "Return the y domain of a drawing area."
  (domain (y-mapping drawing-area)))
