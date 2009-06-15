(in-package :cl-2d)

;;;;  plot and draw functions
;;;;
;;;;  The naming convention is the following: plot-* functions set up
;;;;  and return a new drawing area, while draw-* functions draw on an
;;;;  existing drawing area.


;;;; draw functions

(defun draw-aligned-text (drawing-area x y text 
			  &key (font-style *default-font-style*)
			  (x-align 0.5) (y-align 0.5) (angle 0) (fill-color nil)
			  (padding 2))
  "Draw aligned text at given coordinates."
  (with-slots (x-mapping y-mapping context) drawing-area
    (with-context (context)
      (set-style font-style)
      (let ((text-with-extents (add-text-extents text))
	    (x (map-coordinate x-mapping x))
	    (y (map-coordinate y-mapping y)))
	(when fill-color
	  (aligned-text-rectangle x y text-with-extents fill-color
				  :x-align x-align :y-align y-align :angle angle
				  :padding padding)
	  ;; need to reset color
	  (set-source-color (slot-value font-style 'color)))
	(aligned-text x y text-with-extents
		      :x-align x-align :y-align y-align :angle angle)))))

(defun draw-polygon (drawing-area vertices &key (fill-color +white+)
		     (line-style nil))
  "Draw a polygon with a given fill color and line style.  If either
is nil, the polygon will not be outlined or filled, respectively.
Vertices is either a list of (x y) lists, eg '((1 2) (3 4) (7 9)).
The path is closed at the end.  No sanity checks are performed."
  (with-slots (x-mapping y-mapping context) drawing-area
    (with-context (context)
      (flet ((path ()
	       (iter
		 (for (x y) :in vertices)
		 (for x-c := (map-coordinate x-mapping x))
		 (for y-c := (map-coordinate y-mapping y))
		 (if (first-iteration-p)
		     (move-to x-c y-c)
		     (line-to x-c y-c)))
	       (close-path)))
	(when fill-color
	  (set-source-color fill-color)
	  (path)
	  (fill-path))
	(when line-style
	  (set-style line-style)
	  (path)
	  (stroke))))))

(defun draw-line (drawing-area x-start y-start x-end y-end
		  &optional (line-style *default-line-style*))
  "Draw a line segment between the given coordinates."
  (with-clip-to-frame (drawing-area (width line-style))
    (with-slots (context x-mapping y-mapping) drawing-area
      (with-context (context)
	(with-sync-lock (context)
	  (set-style line-style)
	  (move-to (map-coordinate x-mapping x-start)
		   (map-coordinate y-mapping y-start))
	  (line-to (map-coordinate x-mapping x-end)
		   (map-coordinate y-mapping y-end))
	  (stroke))))
    (values)))

(defun draw-horizontal-line (drawing-area y &optional 
			     (line-style *default-line-style*))
  "Draw a horizontal line at the given coordinate that spans all the
drawing area."
  (let ((x-interval (domain (x-mapping drawing-area))))
    (draw-line drawing-area 
	       (left x-interval) y (right x-interval) y
	       line-style))
  (values))

(defun draw-vertical-line (drawing-area x &optional 
			   (line-style *default-line-style*))
  "Draw a vertical line at the given coordinate that spans all the
drawing area."
  (let ((y-interval (domain (y-mapping drawing-area))))
    (draw-line drawing-area x (left y-interval)
	       x (right y-interval)
	       line-style))
  (values))

(defun draw-lines (drawing-area xs ys &optional (line-style *default-line-style*))
  "Connect the (x,y) coordinates with lines in the order they occur.
The path is not closed.  If nil is found in the ys, the line is broken
there."
  (with-clip-to-frame (drawing-area (width line-style))
    (with-slots (context x-mapping y-mapping) drawing-area
      (with-context (context)
	(with-sync-lock (context)
	  (set-style line-style)
	  (iter
	    (with drawing-line-p := nil)
	    (for x :in-vector xs)
	    (for y :in-vector ys)
	    (for x-d := (map-coordinate x-mapping x))
	    (for y-d := (if y (map-coordinate y-mapping y) nil))
	    (cond
	      ((and (not drawing-line-p) y-d)
	       (move-to x-d y-d)
	     (setf drawing-line-p t))
	    ((and drawing-line-p y-d)
	     (line-to x-d y-d))
	    ((and drawing-line-p (not y-d))
	     (stroke)
	     (setf drawing-line-p nil)))
	  (finally
	   (when drawing-line-p
	     (stroke)))))))
  (values)))


(defun draw-sequence (drawing-area sequence &optional 
		      (line-style *default-line-style*))
  "Draw a sequence, ie values mapped to 0, 1, 2, ..."
  (let* ((vector (coerce sequence 'vector))
	 (length (length vector)))
  (draw-lines drawing-area (num-sequence :from 0 :by 1 :length  length) vector
	      line-style)))

(defun draw-filled-rectangle (drawing-area x1 y1 x2 y2 fill-color 
			      &optional (snap-mode :none))
  "Draw a rectangle with the given fill color and coordinates."
  (with-slots (context x-mapping y-mapping) drawing-area
    (with-context (context)
      (let ((x1 (map-coordinate x-mapping x1 snap-mode))
	    (x2 (map-coordinate x-mapping x2 snap-mode))
	    (y1 (map-coordinate y-mapping y1 snap-mode))
	    (y2 (map-coordinate y-mapping y2 snap-mode)))
	(move-to x1 y1)
	(line-to x1 y2)
	(line-to x2 y2)
	(line-to x2 y1)
	(close-path)
	(set-source-color fill-color)
	(fill-path)))
    (values)))

(defun draw-symbol (drawing-area x y size color symbol-drawing-function)
  "Draw a symbol (using given symbol-drawing-function) at (x,y), with
given size and color."
  (clip-to-frame drawing-area)
  (with-slots (x-mapping y-mapping context) drawing-area
    (with-context (context)
      (with-sync-lock (context)
	(funcall symbol-drawing-function
		 (map-coordinate x-mapping x)
		 (map-coordinate y-mapping y)
		 size 
		 color))
      (reset-clip))))

(defun draw-symbols (drawing-area xs ys &key
		    (weights nil)
		    (symbol-drawing-function #'symbol-hollow-circle)
		    (size-function (proportional-size 4))
		    (color-function (constantly +black+)))
  "Draw points in a given drawing-area, using the given (x,y)
coordinate pairs and optional weights.  The symbols are drawn using
the symbol-drawing-function, with size and color calculated from the
weight using size-function and color-function.  If weights are not
given, 1 is used instead."
  (assert (= (length xs) (length ys)))
  (clip-to-frame drawing-area)
  (with-slots (x-mapping y-mapping context) drawing-area
    (with-context (context)
      (with-sync-lock (context)
	(dotimes (i (length xs))
	  (let ((weight (if weights
			    (aref weights i)
			    1)))
	    (funcall symbol-drawing-function
		     (map-coordinate x-mapping (aref xs i))
		     (map-coordinate y-mapping (aref ys i))
		     (funcall size-function weight)
		     (funcall color-function weight)))))
    (reset-clip))))

(defun draw-function (drawing-area function &key
		      (x-interval (domain (x-mapping drawing-area)))
		      (line-style *default-line-style*)
		      (number-of-points 101)
		      (ignorable-conditions '(division-by-zero)))
  "Draw the function in the given drawing-area.  By default, the interval
is that defined by the drawing-area.  For the definition of
ignorable-conditions, see calculate-function."
  (multiple-value-bind (xs fxs)
      (calculate-function function x-interval number-of-points
			  ignorable-conditions)
    (draw-lines drawing-area xs fxs line-style))
  (values))

(defun draw-histogram (drawing-area histogram &key
		       (line-style *default-line-style*)
		       vertical-lines-p fill-color)
  "Draw the histogram on drawing-area.  vertical-lines-p determines
whether vertical lines are drawn."
  (with-slots (breaks counts) histogram
    (let* ((y-domain (domain (y-mapping drawing-area)))
	   (y-min (left y-domain))
	   (last-index (1- (length counts))))
      (assert (positive-interval-p y-domain))
      ;; plot histogram
      (with-sync-lock ((context drawing-area))
	(with-clip-to-frame (drawing-area)
	  (iter
	    (for i :from 0)
	    (for count :in-vector counts)
	    (let ((left (aref breaks i))
		  (right (aref breaks (1+ i))))
	      ;; fill rectangle with color if fill-color is given
	      (when fill-color
		(draw-filled-rectangle drawing-area left 0 right count fill-color))
	      ;; draw lines if line-style is given
	      (when line-style
		(draw-line drawing-area left count right count line-style)
		(when vertical-lines-p
		  (when (zerop i)
		    (draw-line drawing-area left y-min left count line-style))
		  (let ((larger-count (if (= i last-index)
					  count
					  (max count (aref counts (1+ i))))))
		    (draw-line drawing-area right y-min right larger-count
			       line-style)))))))))))

;;;; plot functions
;;;;
;;;; All of the return drawing area(s) as atom/list.

(defun plot-simple (frame x-interval y-interval
		    &key (x-title "") (y-title "")
		    (simple-plot-style *default-simple-plot-style*)
		    (x-axis (make-instance 'standard-axis
					   :interval x-interval
					   :title x-title))
		    (y-axis (make-instance 'standard-axis
					   :interval y-interval
					   :title y-title))
		    (clear-frame-p t))
  "Create an plot with an empty drawing-area of the given interval."
  (with-slots 
	(frame-padding
	 bottom-axis-size
	 bottom-axis-style
	 left-axis-size
	 left-axis-style) simple-plot-style
    (bind ((internal-frame (pad-frame frame frame-padding))
	   (#2A((nil bottom-axis-frame)
		(left-axis-frame plot-frame)) (split-frame internal-frame 
							   left-axis-size
							   bottom-axis-size))
	   (drawing-area 
	    (setup-drawing-area plot-frame x-interval y-interval)))
      (declare (ignore corner))
      (when clear-frame-p
	(clear frame))
      ;; draw axes
      (left-axis left-axis-frame y-axis left-axis-style)
      (bottom-axis bottom-axis-frame x-axis bottom-axis-style)
      ;; return drawing-area
      drawing-area)))

(defun plot-two-sided (frame x-interval y1-interval y2-interval
		       &key (x-title "") (y1-title "") (y2-title "")
		       (two-sided-plot-style *default-two-sided-plot-style*)
		       (x-axis (make-instance 'standard-axis
					   :interval x-interval
					   :title x-title))
		       (y1-axis (make-instance 'standard-axis
					   :interval y1-interval
					   :title y1-title))
		       (y2-axis (make-instance 'standard-axis
					   :interval y2-interval
					   :title y2-title)))
  "Create an plot with an empty drawing-area of the given intervals, with
y-axis on both the left and right sides, returning two drawing areas
in a list."
  (clear frame)
  (with-slots 
	(frame-padding
	 bottom-axis-size
	 bottom-axis-style
	 left-axis-size
	 left-axis-style
	 right-axis-size
	 right-axis-style) two-sided-plot-style
    (bind ((internal-frame (pad-frame frame frame-padding))
	   (#2A((nil bottom-axis-frame nil)
		(left-axis-frame plot-frame right-axis-frame))
	       (split-frame internal-frame 
			    (list left-axis-size (spacer) right-axis-size)
			    (list bottom-axis-size (spacer))))
	   (drawing-area1
	    (setup-drawing-area plot-frame x-interval y1-interval))
	   (drawing-area2
	    (setup-drawing-area plot-frame x-interval y2-interval)))
      (declare (ignore left-corner right-corner))
      ;; draw axes
      (left-axis left-axis-frame y1-axis left-axis-style)
      (right-axis right-axis-frame y2-axis right-axis-style)
      (bottom-axis bottom-axis-frame x-axis bottom-axis-style)
      ;; return drawing-area
      (list drawing-area1 drawing-area2))))

(defun plot-lines (frame xs ys &key (x-interval (interval-of xs))
		   (y-interval (interval-of ys))
		   (simple-plot-style *default-simple-plot-style*)
		   (line-style *default-line-style*)
		   (x-title "x") (y-title "y"))
  "Create a line plot with the given coordinate pairs."
  ;; create plot
  (let ((drawing-area (plot-simple frame x-interval y-interval
				   :simple-plot-style simple-plot-style
				   :x-title x-title 
				   :y-title y-title)))
    ;; plot function
    (draw-lines drawing-area xs ys line-style)
    ;; return drawing-area
    drawing-area))

(defun plot-lines-two-sided (frame xs y1s y2s &key (x-interval (interval-of xs))
			     (y1-interval (interval-of y1s))
			     (y2-interval (interval-of y2s))
			     (x-title "x")
			     (y1-title "y1")
			     (y2-title "y2")
			     (two-sided-plot-style *default-two-sided-plot-style*)
			     (line-style1 +line-solid+)
			     (line-style2 +line-dash+))
  "Create a plot wth the given coordinate pairs.  Return 
 (list drawing-area1 drawing-area2)."
  (bind (((da1 da2) 
	  (plot-two-sided frame x-interval y1-interval y2-interval
			  :x-title x-title :y1-title y1-title :y2-title y2-title 
			  :two-sided-plot-style two-sided-plot-style)))
    (draw-lines da1 xs y1s line-style1)
    (draw-lines da2 xs y2s line-style2)
    (list da1 da2)))
  
(defun plot-function (frame function x-interval &key
		      (y-interval #'interval-of)
		      (simple-plot-style *default-simple-plot-style*)
		      (line-style *default-line-style*)
		      (number-of-points 101)
		      (x-title "x") (y-title (format nil "f(~a)" x-title))
		      (ignorable-conditions '(division-by-zero)))
  "Set up a plot and draw the function in the given interval.  If the
y-lower and y-upper endpoints are not specified, they are calculated
automatically by calling y-interval on the y values.  For the
intepretation of ignorable-conditions, see calculate-function."
  ;; calculate function values
  (multiple-value-bind (xs fxs)
      (calculate-function function x-interval number-of-points
			  ignorable-conditions)
    (let ((y-interval (funcall y-interval fxs)))
      ;; create plot
      (plot-lines frame xs fxs :x-interval x-interval :y-interval y-interval
		  :simple-plot-style simple-plot-style
		  :line-style line-style
		  :x-title x-title
		  :y-title y-title))))

(defun plot-sequence (frame sequence &key
		      (y-interval (interval-of sequence))
		      (simple-plot-style *default-simple-plot-style*)
		      (line-style *default-line-style*)
		      (x-title "N") (y-title "sequence"))
  "Plot a sequence, ie values mapped to 0, 1, 2, ..."
  (let* ((vector (coerce sequence 'vector))
	 (length (length vector)))
    (plot-lines frame (num-sequence :from 0 :by 1 :length length) vector
		:x-interval (make-interval 0 (1- length)) :y-interval y-interval
		:simple-plot-style simple-plot-style
		:line-style line-style :x-title x-title
		:y-title y-title)))

(defun create-boundaries (x conv interval)
  "Create boundaries for rectangles in an image plot.  Boundaries are
restricted to interval."
;  (declare ((array * (*)) x))		; !!! real
  (let* ((lower (left interval))
	 (upper (right interval))
	 (length (length x))
	 (boundaries (make-array (1+ length) :element-type 'real)))
    (flet ((into-interval (v)
	     (min upper (max lower v))))
      (setf (aref boundaries 0) (funcall conv (into-interval (aref x 0))))
      (iter
	(for i from 1 below length)
	(setf (aref boundaries i) 
	      (funcall conv (into-interval (/ (+ (aref x (1- i)) (aref x i)) 2))
		       :int)))		; snap to integer
      (setf (aref boundaries length)
	    (funcall conv (into-interval (aref x (1- length))))))
      boundaries))

(defun plot-image (frame x y z &key (color-mapping #'make-hue-color-mapping)
		   (plot-style *default-image-plot-style*)
		   (image-legend-style *default-image-legend-style*)
		   (legend-width 70)
		   (x-title "")
		   (y-title "")
		   (z-title "")
		   (x-interval (interval-of x))
		   (y-interval (interval-of y)))
  (declare ((array * (*)) x y))		; !!! real
  (declare ((array * (* *)) z))		; !!! real
  ;; if color mapping is a function, use it to calculate color mapping
  (when (functionp color-mapping)
    (setf color-mapping (funcall color-mapping (interval-of z))))
  (clear frame)
  (with-slots (color-function) color-mapping
    (bind ((#(plot-frame legend-frame)
	     (split-frame-horizontally frame (spacer) legend-width))
	   ;; draw plot frame
	   (drawing-area 
	    (plot-simple plot-frame
			 x-interval y-interval
			 :simple-plot-style plot-style
			 :x-title x-title :y-title y-title
			 :clear-frame-p nil)))
      ;; plot image
      (with-slots (context x-mapping y-mapping) drawing-area
	(let ((x-boundaries (create-boundaries x (conversion x-mapping) x-interval))
	      (y-boundaries (create-boundaries y (conversion y-mapping) y-interval)))
	  (with-sync-lock (context)
	    (dotimes (i (length x))
	      (dotimes (j (length y))
		(let ((x1 (aref x-boundaries i))
		      (y1 (aref y-boundaries j))
		      (x2 (aref x-boundaries (1+ i)))
		      (y2 (aref y-boundaries (1+ j))))
		  (when (and (< x1 x2) (> y1 y2))
		    (filled-rectangle (funcall color-function (aref z i j))
				      x1 y1 x2 y2 context))))))))
      ;; draw legend
      (image-legend legend-frame color-mapping
		    :image-legend-style image-legend-style :z-title z-title)
      ;; return drawing-area
      drawing-area)))


(defun plot-histogram (frame histogram &key
		       (simple-plot-style *default-simple-plot-style*)
		       (line-style *default-line-style*)
		       (x-title "x") (y-title "count") vertical-lines-p
		       (x-interval (interval-of (slot-value histogram 'breaks)))
		       (fill-color nil))
  "Plot the histogram on frame, return the resulting drawing area.
For the meaning of parameters, see draw-histogram."
  ;; create plot
  (with-slots (breaks counts) histogram
    (let ((drawing-area (plot-simple frame 
				     x-interval
				     (interval-of counts 0)
				     :simple-plot-style simple-plot-style
				     :x-title x-title 
				     :y-title y-title)))
      (draw-histogram drawing-area histogram
		      :line-style line-style
		      :vertical-lines-p vertical-lines-p :fill-color fill-color)
      ;; return drawing-area
      drawing-area)))

