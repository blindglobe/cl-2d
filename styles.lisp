(in-package :cl-2d)

(defgeneric copy-style (object)
  (:documentation "Copy a style object.  Not necessarily defined for
  all style classes, usually implements a shallow copy."))

(defun modify-style (style &rest slot-value-pairs)
  "Make a copy of style and replace the given slots with the given
values.  Example: (modify-slots line-style :width 2 :color +red+."
  (let ((style (copy-style style)))
    (iter
      (for s-v-pair :on slot-value-pairs :by #'cddr)
      (for (slot value) := s-v-pair)
      (for slot-in-package := (find-symbol (symbol-name slot) :cl-2d))
      (unless slot-in-package
	(error "~a is not a valid symbol in cl-2d" slot))
      (unless value
	(error "you did not specify a value for ~a" slot))
      (setf (slot-value style slot-in-package) value))
    style))

(defclass font-style () ((name :initform "Arial" :initarg :name)
			 (slant :initform :normal
				:initarg :slant)
			 (weight :initform :normal
				 :initarg :weight)
			 (size :initform 12d0 :initarg :size)
			 (color :initform +black+ :initarg :color)))

(defmethod copy-style ((style font-style))
  (with-slots (name slant weight size color) style
    (make-instance 'font-style :name name :slant slant :weight weight
		   :size size :color color)))

(defclass dash-style ()  
  ((offset :initform 0d0 :initarg :offset :type double-float)
   (dashes :initform #() :initarg :dashes)))

(defmethod copy-style ((style dash-style))
  (with-slots (offset dashes) style
    (make-instance 'dash-style :offset offset :dashes dashes)))
   
(defclass line-style ()
  ((width :initform 1 :initarg :width :accessor width)
   (color :initform +black+ :initarg :color)
   (dash-style :initform (make-instance 'dash-style) :initarg :dash-style)))

(defmethod copy-style ((style line-style))
  (with-slots (width color dash-style) style
    (make-instance 'line-style :width width :color color :dash-style dash-style)))

(defparameter *default-font-style* (make-instance 'font-style))
(defparameter *default-title-font-style* (make-instance 'font-style
							:size 20
							:weight :bold))

;;;; Predefined styles below are actually constants, and named
;;;; accordingly (with +'s), but it is more appropriate to define them
;;;; with 

(defparameter +dash-solid+ (make-instance 'dash-style))
(defparameter +dash-dash+ (make-instance 'dash-style :dashes #(4 4)))
(defparameter +dash-dot+ (make-instance 'dash-style :dashes #(1 2)))
(defparameter +dash-dot-dash+ (make-instance 'dash-style :dashes #(1 2 4 2)))

(defparameter *default-dash-style* +dash-solid+)

(defparameter +line-solid+ (make-instance 'line-style))
(defparameter +line-dash+ (make-instance 'line-style :dash-style +dash-dash+))
(defparameter +line-dot+ (make-instance 'line-style :dash-style +dash-dot+))
(defparameter +line-dot-dash+ (make-instance 'line-style :dash-style
					     +dash-dot-dash+))

(defparameter *default-line-style* +line-solid+)

(defclass axis-style ()
  ((axis-padding
    :initform 3
    :initarg :axis-padding
    :documentation "padding between frame edge and axis")
   (font-style
    :initform (make-instance 'font-style)
    :initarg :font-style
    :documentation "axis font style")
   (line-style
    :initform (make-instance 'line-style)
    :initarg :line-style
    :documentation "axis line style")
   (tick-length 
    :initform 3 
    :initarg :tick-length
    :documentation "length of a tick mark")
   (tick-padding
    :initform 3
    :initarg :tick-padding
    :documentation "padding between tick mark and number")
   (title-padding
    :initform 5
    :initarg :tick-padding
    :documentation "padding between edge of frame and axis title")
   (label-orientation
    :initform 'parallel
    :initarg :label-orientation
    :accessor label-orientation
    :documentation "orientation of labels (can be parallel, perpendicular,
horizontal, vertical")))

(defparameter *default-axis-style* (make-instance 'axis-style))

(defclass image-legend-style ()
  ((axis-style
    :initform (make-instance 'axis-style)
    :initarg :font-style
    :documentation "axis style")
   (width
    :initform 15
    :initarg :width
    :documentation "width of the color bar")
   (number-of-colors
    :initform 50
    :initarg :number-of-colors
    :documentation "number of colors in the color bar")
   (padding
    :initform (make-instance 'padding :left 0.5 :top 20.5 :bottom 20.5 :right 9.5)
    :initarg :padding
    :documentation "padding around legend")
   (axis-label-distance
    :initform 2
    :initarg :axis-label-distance
    :documentation "distance of label from edge of padded frame")))

(defparameter *default-image-legend-style* (make-instance 'image-legend-style))

(defclass simple-plot-style ()
  ((frame-padding
    :initform (make-instance 'padding :left 2.5 :top 19.5 :bottom 2.5 :right 19.5)
    :initarg :frame-padding
    :documentation "padding of the frame")
   (bottom-axis-size
    :initform 40
    :initarg :bottom-axis-size
    :documentation "space reserved for bottom axis (incl labels)")
   (bottom-axis-style
    :initform (make-instance 'axis-style)
    :initarg :bottom-axis-style
    :documentation "style of bottom axis")
   (left-axis-size
    :initform 40
    :initarg :left-axis-size
    :documentation "space reserved for left axis (incl labels)")
   (left-axis-style
    :initform (make-instance 'axis-style)
    :initarg :left-axis-style
    :documentation "style of left axis")))

(defparameter *default-simple-plot-style* (make-instance 'simple-plot-style))
(defparameter *default-image-plot-style*
  (make-instance 'simple-plot-style :frame-padding
		 (make-instance 'padding
				:left 2.5 :top 19.5
				:bottom 2.5 :right 10.5))
  "padding for image plots (in the image frame area")
		 



(defclass two-sided-plot-style (simple-plot-style)
  ((right-axis-size
    :initform 40
    :initarg :right-axis-size
    :documentation "space reserved for right axis (incl labels)")
   (right-axis-style
    :initform (make-instance 'axis-style)
    :initarg :right-axis-style
    :documentation "style of right axis")))

(defparameter *default-two-sided-plot-style* (make-instance 'two-sided-plot-style))

(defclass horizontal-legend-style ()
  ((font-style :initarg :font-style :initform (make-instance 'font-style))
   (left-padding :initarg :left-padding :initform 10)
   (style-left-padding :initarg :style-left-padding :initform 20)
   (style-text-padding :initarg :style-text-padding :initform 10)
   (text-right-padding :initarg :text-right-padding :initform 5)
   (line-length :initarg :line-length :initform 20)))

(defparameter *default-horizontal-legend-style* 
  (make-instance 'horizontal-legend-style))

(defgeneric set-style (style &optional context))

(defmethod set-style ((style font-style) &optional (context *context*))
  "Set the font style from the given style."
  (with-slots (name slant weight size color) style
    (with-context (context)
      (select-font-face name slant weight)
      (set-font-size size)
      (set-source-color color))))

(defmethod set-style ((style dash-style) &optional (context *context*))
  "Set the dash style from the given style."
  (with-slots (offset dashes) style
    (set-dash offset dashes context)))

(defmethod set-style ((style line-style) &optional (context *context*))
  "Set the line style from the given style."
  (with-slots (width color dash-style) style
    (with-context (context)
      (set-source-color color)
      (set-line-width width)
      (set-style dash-style))))
