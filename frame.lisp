(in-package :cl-2d)

;;;;  Frames
;;;;  
;;;;  Frames are simply a rectangular portion of a Cairo context.
;;;;  They are central to the architecture of cl-2d: all plotting
;;;;  functions are designed to operate in a frame.  Frames can be
;;;;  subdivided to create new frames.

;;;;  Important note on coordinates: in Cairo's coordinate system, the
;;;;  positive directions are to the right and DOWN.  In cl-2d's, they
;;;;  are to the right and UP.  Frames should be handled with this in
;;;;  mind.

(defclass frame ()
  (;; coordinates in cairo-space
   (horizontal-interval :initarg :horizontal-interval :type interval
			:reader horizontal-interval)
   (vertical-interval :initarg :vertical-interval :type interval
		      :reader vertical-interval)
   ;; context
   (context :initarg :context :reader context)
   ;; background color - if non-nil, frame is cleared with this before plots
   (background-color :initarg :background-color :reader
		     background-color)))

(defmethod initialize-instance :after ((frame frame) &key)
  (with-slots (horizontal-interval vertical-interval) frame
    (assert (and (weakly-positive-interval? horizontal-interval)
		 (weakly-negative-interval? vertical-interval)))))

(defmethod print-object ((obj frame) stream)
  "Print a frame object."
  (print-unreadable-object (obj stream :type t)
    (with-slots (horizontal-interval vertical-interval context background-color) obj
      (format stream "horizontal-interval: ~a, vertical-interval: ~a, context: ~a,~
background-color: ~a"
	      horizontal-interval vertical-interval context background-color))))

(defmethod width ((frame frame))
  (width (horizontal-interval frame)))

(defmethod height ((frame frame))
  (width (vertical-interval frame)))

;;;;
;;;;  conversion
;;;;

(defgeneric as-frame (object &key &allow-other-keys)
  (:documentation "Create a frame from object."))

(defmethod as-frame ((object context) &key (background-color nil))
  (make-instance 'frame
		 :horizontal-interval (make-interval 0 (width object))
		 :vertical-interval (make-interval (height object) 0)
		 :context object
		 :background-color background-color))

(defmethod as-frame ((object xlib-image-context) &key (background-color +white+))
  (make-instance 'frame
		 :horizontal-interval (make-interval 0 (width object))
		 :vertical-interval (make-interval (height object) 0)
		 :context object
		 :background-color (and background-color 
                                        (slot-value object 
                                                    'cl-cairo2::background-color))))

(defmethod as-frame ((object gtk2-xlib-context)
                     &key (background-color +white+))
  (make-instance 'frame
		 :horizontal-interval (make-interval 0 (width object))
		 :vertical-interval (make-interval (height object) 0)
		 :context object
		 :background-color background-color))


;;;;
;;;; frame manipulations
;;;;

(defun split-frame (frame divx divy)
  "Split the frame into a grid define by the subdivision sequences
divx and divy (see split-interval for semantics).  Return a 2D array.

Note: xs or ys can be nil, in that case the frame is split in one
direction only.  If xs or ys is an atom, it is converted into a vector
with a single element."
  (flet ((seq% (x)
	   (cond
	     ((null x) nil)
	     ((vectorp x) x)
	     ((listp x) x)
	     ((atom x) (vector x)))))
    (with-slots (horizontal-interval vertical-interval
				     context background-color) frame
      (let ((xints (split-interval horizontal-interval (seq% divx)))
	    (yints (split-interval vertical-interval (seq% divy))))
	(xop 'array
             (lambda (yint xint)
               (make-instance 'frame 
                              :horizontal-interval xint
                              :vertical-interval yint
                              :context context 
                              :background-color 
                              background-color))
              yints xints)))))	; order is important!

(defun split-frame-horizontally (frame &rest divx)
  "Split the frame at subdivisions divx.  The resulting frames are
returned as a list."
  (as 'array (drop (split-frame frame divx nil))))

(defun split-frame-vertically (frame &rest divy)
  "Split the frame at subdivisions divy.  The resulting frames are
returned as a list."
  (as 'array (drop (split-frame frame nil divy))))

(defclass padding ()
  ((left :initarg :left)
   (top :initarg :top)
   (bottom :initarg :bottom)
   (right :initarg :right)))

(defun pad-frame (frame padding)
  "Shrink frame with padding."
  (with-slots (left top bottom right) padding
    (aref (split-frame frame
		       (vector left (spacer) right)
		       (vector bottom (spacer) top))
	  1 1)))

;;;;
;;;;  clipping for frames
;;;;

(defun clip-to-frame (frame &optional (line-width 0))
  "Set cairo clip to the frame.  Clip is expanded by width/2, for
accommodating lines with the given width."
  (let ((half-width (/ line-width 2)))
    (with-slots (horizontal-interval vertical-interval context) frame
      (with-context (context)
	(reset-clip)
	(rectangle (- (interval-left horizontal-interval) half-width)
		   (- (interval-right vertical-interval) half-width)
		   (+ line-width (interval-width horizontal-interval))
		   (+ line-width (interval-width vertical-interval)))
	(clip)))))

(defmacro with-clip-to-frame ((frame &optional (line-width 0)) &body body)
  "Execute body with clipping to frame, resetting clip afterwards.
Protected from nonlocal exits."
  (once-only (frame)
    `(progn
       (clip-to-frame ,frame ,line-width)
       (unwind-protect (progn ,@body)
	 (reset-clip (slot-value ,frame 'context))))))

;;;;
;;;;  clearing frames and contexts with background-color
;;;;


(defgeneric fill-with-color (object color)
  (:documentation "fill the object with the given color"))

(defmethod fill-with-color ((frame frame) color)
  (with-slots (horizontal-interval vertical-interval context) frame
;;    (reset-clip context)
    (filled-rectangle color
		      (interval-left horizontal-interval)
		      (interval-right vertical-interval)
		      (interval-right horizontal-interval)
		      (interval-left vertical-interval)
		      context)))

(defun clear (frame)
  "Fills frame with its background-color.  Doesn't make much sense for
graphics which goes into a file (PS, PDF), but useful for reusing
on-screen frames (eg x11) without recreating them anew.  If
background-color of frame is nil, this function has no effect (as it
is usually the case for PDF, etc)."
  (with-slots (background-color) frame
    (when background-color
      (fill-with-color frame background-color))))
