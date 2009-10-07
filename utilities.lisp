(in-package :cl-2d)

;;;;  snapping
;;;;  
;;;;  Snapping to integers and integers+0.5 makes sure that lines and
;;;;  blocks look nice on pixel-based surfaces.  Use only when
;;;;  necessary.

(declaim (inline snap))

(defun snap (value mode)
  "Snapping has 3 modes:
   :none    value is passed through
   :int     value is rounded to the nearest integer
   :half    value is rounded to the nearest integer+0.5

On borders (int+0.5 for :int, int for :half) they round up."
  (ecase mode
    (:none value)
    (:int (round value))
    (:half (+ (floor value) 0.5))))

(defun snap* (value mode &optional (context *context*))
  "Snap based on whether context is pixel-based."
  (if (pixel-based-p context)
      (snap value mode)
      value))

;;;;  primitives
;;;;
;;;;  Primitives are functions that use only cairo directly, for
;;;;  drawing on a context or setting up paths, etc.  Thus, they have
;;;;  the context as an optional argument, with *context* as the
;;;;  default.

(defun segment (x-start y-start x-end y-end &optional (context *context*))
  "Draw a line from (x-start,y-start) to (x-end,y-end)."
  (with-context (context)
    (move-to x-start y-start)
    (line-to x-end y-end)
    (stroke)))

(defun filled-rectangle (color left top right bottom &optional (context *context*))
  "Fill the rectangle defined by the coordinates with color."
  (with-context (context)
    (rectangle left top (- right left) (- bottom top))
    (set-source-color color)
    (fill-path)))

(defun circle-path (x y radius &optional (context *context*))
  "Set up a path for a circle centered at (x,y) with given radius."
  (with-context (context)
    (move-to (+ x radius) y)
    (arc x y radius 0 (twice pi))))

(defun filled-circle (x y radius color &optional (context *context*))
  "Draw a filled circle with the given color, center (x,y) and radius."
  (with-context (context)
    (circle-path x y radius)
    (set-source-color color)
    (fill-path)))

;;;;  text handling
;;;;
;;;;  Text handling relies on the ability of Cairo to measure the
;;;;  extents of a text, which are then aligned properly.

(defclass text-with-extents ()
  ((text :initarg :text)
   (x-bearing :initarg :x-bearing)
   (y-bearing :initarg :y-bearing)
   (width :initarg :width)
   (height :initarg :height)
   (x-advance :initarg :x-advance)
   (y-advance :initarg :y-advance)))

(defun add-text-extents (text &optional (context *context*))
  (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
      (text-extents text context)
    (make-instance 'text-with-extents
     :text text
     :x-bearing x-bearing
     :y-bearing y-bearing
     :width width
     :height height
     :x-advance x-advance
     :y-advance y-advance)))

(defun capital-letter-height (&optional (context *context*))
  "Return the height of a typical capital letter, currently H."
  (multiple-value-bind (x-bearing y-bearing width height)
      (text-extents "H" context)
    (declare (ignore x-bearing y-bearing width))
    height))

(defun alignment-rotation (x-align y-align angle)
  "Return the after-rotation alignment as values."
  (let ((x-align (- x-align 0.5))
	(y-align (- y-align 0.5)))
    (values (+ 0.5 (* (cos angle) x-align) (- (* (sin angle) y-align)))
	    (+ 0.5 (* (sin angle) x-align) (* (cos angle) y-align)))))

(defgeneric aligned-text (x y text &key x-align y-align angle context
			    rotate-after-p))
  
(defmethod aligned-text (x y (text-with-extents text-with-extents)
			 &key (x-align 0.5) (y-align 0.5) (angle 0)
			 (context *context*) (rotate-after-p t))
  "Show text-with-extents aligned relative to (x,y).  Return width and
height."
  (with-context (context)
    (with-slots (text x-bearing y-bearing width height) text-with-extents
      (when rotate-after-p
      	(setf (values x-align y-align)
      	      (alignment-rotation x-align y-align (- angle))))
      (bind ((trans-matrix (get-trans-matrix))
	     (x-rel (+ x-bearing (* width x-align)))
	     (y-rel (+ y-bearing (* height y-align))))
	;; show text
	(move-to x y)
	(rotate angle)
	(rel-move-to (- 0 x-rel) (- 0 y-rel))
	(show-text text)
	(set-trans-matrix trans-matrix))
      (values width height))))

(defmethod aligned-text-rectangle (x y text-with-extents fill-color
				   &key (x-align 0.5) (y-align 0.5) (angle 0)
				   (padding 2) (context *context*))
  "Fill a rectangle corresponding to text-with-extents and the given
alignment with fill-color."
  (with-context (context)
    (with-slots (text x-bearing y-bearing width height) text-with-extents
      (move-to x y)
      (let ((trans-matrix (get-trans-matrix))
	    (x-rel (+ x-bearing (* width x-align) padding))
	    (y-rel (- (+ y-bearing (* height y-align)) padding))
	    (width (+ (* 2 padding) width))
	    (height (+ (* 2 padding) height)))
	(rotate angle)
	(rel-move-to (- 0 x-rel) (- 0 y-rel))
	(rel-line-to width 0d0)
	(rel-line-to 0d0 (- height))
	(rel-line-to (- width) 0d0)
	(close-path)
	(set-source-color fill-color)
	(fill-path)
	(set-trans-matrix trans-matrix))
      (values width height))))
  
(defmethod aligned-text (x y (text string)
			 &key (x-align 0.5) (y-align 0.5) (angle 0)
			 (context *context*) (rotate-after-p t))
  "Show text aligned relative to (x,y).  Return width and height."
  (with-context (context)
    (aligned-text x y (add-text-extents text)
		  :x-align x-align :y-align y-align :angle angle
		  :rotate-after-p rotate-after-p)))
