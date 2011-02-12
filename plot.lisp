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
  (when line-style
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
      (values))))

(defun draw-horizontal-line (drawing-area y &optional 
			     (line-style *default-line-style*))
  "Draw a horizontal line at the given coordinate that spans all the
drawing area."
  (let ((x-interval (domain (x-mapping drawing-area))))
    (draw-line drawing-area 
	       (interval-left x-interval) y (interval-right x-interval) y
	       line-style))
  (values))

(defun draw-vertical-line (drawing-area x &optional 
			   (line-style *default-line-style*))
  "Draw a vertical line at the given coordinate that spans all the
drawing area."
  (let ((y-interval (domain (y-mapping drawing-area))))
    (draw-line drawing-area x (interval-left y-interval)
	       x (interval-right y-interval)
	       line-style))
  (values))

(defun draw-regression-line (da intercept slope &optional
                             (line-style *default-line-style*))
  "Draw a line with given intercept and slope."
  (bind (((:interval left right) (x-domain da)))
    (with-clip-to-frame (da)
      (draw-line da 
                 left (+ intercept (* slope left))
                 right (+ intercept (* slope right))
                 line-style))))

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
	    (for x :in-vector (coerce xs 'vector))
	    (for y :in-vector (coerce ys 'vector))
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
  (draw-lines drawing-area (numseq 0 1 :length length) vector
	      line-style)))

(defun draw-rectangle (drawing-area x1 y1 x2 y2 
                       &key fill-color line-style (snap-mode :none))
  "Draw a rectangle with the given fill color, line style (either can be nil)
and coordinates."
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
        (when fill-color
          (set-source-color fill-color)
          (fill-path))
        (when line-style
          (set-style line-style)
          (stroke))))
    (values)))

(defun draw-circle (drawing-area x y radius
                       &key fill-color line-style (snap-mode :none))
  "Draw a circle with the given fill color, line style (either can be nil),
coordinates and radius."
  (unless (or fill-color line-style)
    (return-from draw-circle (values)))
  (with-slots (context x-mapping y-mapping) drawing-area
    (with-context (context)
      (let ((x (map-coordinate x-mapping x snap-mode))
	    (y (map-coordinate y-mapping y snap-mode)))
        (circle-path x y radius)
        (when fill-color
          (set-source-color fill-color)
          (fill-path))
        (when line-style
          (set-style line-style)
          (stroke))))
    (values)))


(defun draw-symbol (drawing-area x y symbol-drawing-function &rest parameters)
  "Draw a symbol (using given symbol-drawing-function) at (x,y), with given parameters."
  (clip-to-frame drawing-area)
  (with-slots (x-mapping y-mapping context) drawing-area
    (with-context (context)
      (with-sync-lock (context)
	(apply symbol-drawing-function
               (map-coordinate x-mapping x)
               (map-coordinate y-mapping y)
               parameters))
      (reset-clip))))

(defun draw-symbols (drawing-area xs ys &key
                     (symbol-drawing-function #'symbol-hollow-circle)
                     (size 4) (color +black+) (label ""))
  "Draw points in a given drawing-area, using the given (x,y)
coordinate pairs and optional weights.  The symbols are drawn using
the symbol-drawing-function, with size and color calculated from the
weight using size and color.  If weights are not
given, 1 is used instead."
  (assert (= (length xs) (length ys)))
  (clip-to-frame drawing-area)
  (with-slots (x-mapping y-mapping context) drawing-area
    (with-context (context)
      (with-sync-lock (context)
	(dotimes (i (length xs))
	  (bind (((:flet pick (argument))
                  (if (vectorp argument)
                      (aref argument i)
                      argument)))
	    (funcall symbol-drawing-function
                     (map-coordinate x-mapping (aref xs i))
                     (map-coordinate y-mapping (aref ys i))
                     :size (pick size)
                     :color (pick color)
                     :label (pick label)))))
    (reset-clip))))

;;;; auxiliary functions
;;;;

(defun calculate-function (function domain number-of-points
			   ignorable-conditions)
  "Calculate function at number-of-points points on domain.  If a
member of ignorable-conditions is encountered, the condition is
handled and the value is nil.  Return (values xs fxs)."
  (assert (>= number-of-points 2))
  (let ((xs (numseq (interval-left domain)
                    (interval-right domain)
                    :length number-of-points
                    :type 'real))
	(fxs (make-array number-of-points))
	(caught-condition-p nil))
    (iter
      (for i :from 0)
      (for x :in-vector xs)
      (for fx := (handler-case (funcall function x)
		   (t (condition)
		     (if (member condition ignorable-conditions
				 :test (function typep))
			 nil
			 (error condition)))))
      (setf (aref fxs i) fx)
      (unless fx
	(setf caught-condition-p t)))
    (values xs fxs)))

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
	   (y-min (interval-left y-domain))
	   (last-index (1- (length counts))))
      (assert (positive-interval? y-domain))
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
		(draw-rectangle drawing-area left 0 right count
                                :fill-color fill-color))
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
;;;; All of these functions return drawing area(s) as atoms (single
;;;; one) or lists.

(defun plot-simple (frame x-interval y-interval
		    &key (x-title "") (y-title "")
		    (x-mapping-type 'linear-mapping)
		    (y-mapping-type 'linear-mapping)
		    (x-axis t)
		    (y-axis t)
		    (simple-plot-style *default-simple-plot-style*))
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
	    (setup-drawing-area plot-frame x-interval y-interval
				:x-mapping-type x-mapping-type
				:y-mapping-type y-mapping-type)))
      (declare (ignore corner))
      (clear frame)
      ;; draw axes
      (when y-axis
        (left-axis left-axis-frame (y-mapping drawing-area) y-axis y-title
                   left-axis-style))
      (when x-axis
        (bottom-axis bottom-axis-frame (x-mapping drawing-area) x-axis x-title
                     bottom-axis-style))
      ;; return drawing-area
      drawing-area)))

(defun plot-two-sided (frame x-interval y1-interval y2-interval
		       &key (x-title "") (y1-title "") (y2-title "")
		       (x-mapping-type 'linear-mapping)
		       (y1-mapping-type 'linear-mapping)
		       (y2-mapping-type 'linear-mapping)
		       (x-axis t)
		       (y1-axis t)
		       (y2-axis t)
		       (two-sided-plot-style *default-two-sided-plot-style*))
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
	    (setup-drawing-area plot-frame x-interval y1-interval
				:x-mapping-type x-mapping-type
				:y-mapping-type y1-mapping-type))
	   (drawing-area2
	    (setup-drawing-area plot-frame x-interval y2-interval
				:x-mapping-type x-mapping-type
				:y-mapping-type y2-mapping-type)))
      (declare (ignore left-corner right-corner))
      ;; draw axes
      (left-axis left-axis-frame (y-mapping drawing-area1) y1-axis y1-title
		 left-axis-style)
      (right-axis right-axis-frame (y-mapping drawing-area1) y2-axis y2-title
		  right-axis-style)
      (bottom-axis bottom-axis-frame (x-mapping drawing-area1) x-axis x-title
		   bottom-axis-style)
      ;; return drawing-area
      (list drawing-area1 drawing-area2))))

(defun plot-lines (frame xs ys &key (x-interval (range xs))
		   (y-interval (range ys))
		   (x-title "x") (y-title "y")
		   (x-mapping-type 'linear-mapping)
		   (y-mapping-type 'linear-mapping)
		   (x-axis t)
		   (y-axis t)
		   (simple-plot-style *default-simple-plot-style*)
		   (line-style *default-line-style*))
  "Create a line plot with the given coordinate pairs."
  ;; create plot
  (let ((drawing-area (plot-simple frame x-interval y-interval
				   :x-title x-title 
				   :y-title y-title
				   :simple-plot-style simple-plot-style
				   :x-mapping-type x-mapping-type
				   :y-mapping-type y-mapping-type :x-axis x-axis
				   :y-axis y-axis)))
    ;; plot function
    (draw-lines drawing-area xs ys line-style)
    ;; return drawing-area
    drawing-area))

(defun plot-lines-two-sided (frame xs y1s y2s &key (x-interval (range xs))
			     (y1-interval (range y1s))
			     (y2-interval (range y2s))
			     (x-title "x")
			     (y1-title "y1")
			     (y2-title "y2")
			     (x-mapping-type 'linear-mapping)
			     (y1-mapping-type 'linear-mapping)
			     (y2-mapping-type 'linear-mapping)
			     (x-axis t)
			     (y1-axis t)
			     (y2-axis t)
			     (two-sided-plot-style *default-two-sided-plot-style*)
			     (line-style1 +line-solid+)
			     (line-style2 +line-dash+))
  "Create a plot wth the given coordinate pairs.  Return 
 (list drawing-area1 drawing-area2)."
  (bind (((da1 da2) 
	  (plot-two-sided frame x-interval y1-interval y2-interval
			  :x-title x-title :y1-title y1-title :y2-title y2-title 
			  :x-mapping-type x-mapping-type
			  :y1-mapping-type y1-mapping-type
			  :y2-mapping-type y2-mapping-type
			  :x-axis x-axis :y1-axis y1-axis :y2-axis y2-axis
			  :two-sided-plot-style two-sided-plot-style)))
    (draw-lines da1 xs y1s line-style1)
    (draw-lines da2 xs y2s line-style2)
    (list da1 da2)))

(defun plot-symbols (frame xs ys &key
                     (x-interval (range xs))
                     (y-interval (range ys))
                     (x-title "x") (y-title "y")
                     (x-mapping-type 'linear-mapping)
                     (y-mapping-type 'linear-mapping)
                     (x-axis t)
                     (y-axis t)
                     (simple-plot-style *default-simple-plot-style*)
                     (symbol-drawing-function #'symbol-hollow-circle)
                     (size 10)
                     (color +black+)
                     label)
  (let ((drawing-area (plot-simple frame x-interval y-interval
				   :x-title x-title 
				   :y-title y-title
				   :simple-plot-style simple-plot-style
				   :x-mapping-type x-mapping-type
				   :y-mapping-type y-mapping-type :x-axis x-axis
				   :y-axis y-axis)))
    (draw-symbols drawing-area xs ys
                  :symbol-drawing-function symbol-drawing-function
                  :size size :color color :label label)
    drawing-area))
  
(defun plot-function (frame function x-interval &key
		      (y-interval nil)
		      (x-title "x") (y-title (format nil "f(~a)" x-title))
		      (x-mapping-type 'linear-mapping)
		      (y-mapping-type 'linear-mapping)
		      (x-axis t)
		      (y-axis t)
		      (simple-plot-style *default-simple-plot-style*)
		      (line-style *default-line-style*)
		      (number-of-points 101)
		      (ignorable-conditions '(division-by-zero)))
  "Set up a plot and draw the function in the interval (range ys
y-interval), ie NIL is ignored, points are incorporated, use
FORCED-INTERVAL to force a particular interval.  For the intepretation
of ignorable-conditions, see calculate-function."
  ;; calculate function values
  (multiple-value-bind (xs fxs)
      (calculate-function function x-interval number-of-points
			  ignorable-conditions)
    (let ((y-interval (combined-range fxs y-interval)))
      ;; create plot
      (plot-lines frame xs fxs :x-interval x-interval :y-interval y-interval
		  :x-title x-title :y-title y-title :x-mapping-type x-mapping-type
		  :y-mapping-type y-mapping-type :x-axis x-axis :y-axis y-axis
		  :simple-plot-style simple-plot-style :line-style line-style))))

(defun plot-sequence (frame sequence &key
                      (x (numseq 0 (1- (length sequence)) :type 'fixnum))
		      (x-interval (range x))
		      (y-interval (range sequence))
		      (x-title "N") (y-title "sequence")
		      (x-mapping-type 'linear-mapping)
		      (y-mapping-type 'linear-mapping)
		      (x-axis t) (y-axis t)
		      (simple-plot-style *default-simple-plot-style*)
		      (line-style *default-line-style*))
  "Plot a sequence, ie values mapped to 0, 1, 2, ..."
  (let* ((vector (coerce sequence 'vector)))
    (plot-lines frame x vector
		:x-interval x-interval :y-interval y-interval
		:x-title x-title :y-title y-title :x-mapping-type x-mapping-type
		:y-mapping-type y-mapping-type :x-axis x-axis :y-axis y-axis
		:simple-plot-style simple-plot-style :line-style line-style)))

(defun create-boundaries (x conv interval)
  "Create boundaries for rectangles in an image plot.  Boundaries are
restricted to interval."
;  (declare ((array * (*)) x))		; !!! real
  (bind (((:interval lower upper) interval)
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
		   (x-interval (range x))
		   (y-interval (range y))
		   (x-title "x")
		   (y-title "y")
		   (z-title "z")
		   (x-mapping-type 'linear-mapping)
		   (y-mapping-type 'linear-mapping)
		   (x-axis t)
		   (y-axis t)
		   (plot-style *default-image-plot-style*)
		   (image-legend-style *default-image-legend-style*)
		   (legend-width 70))
  (declare (optimize debug))
  (declare (type (array * (*)) x y))
  (declare (type (array * (* *)) z))
  ;; if color mapping is a function, use it to calculate color mapping
  (when (functionp color-mapping)
    (setf color-mapping (funcall color-mapping (range z))))
  (clear frame)
  (with-slots (color-function) color-mapping
    (bind ((#(plot-frame legend-frame)
	     (split-frame-horizontally frame (spacer) legend-width))
	   ;; draw plot frame
	   (drawing-area 
	    (plot-simple plot-frame
			 x-interval y-interval
			 :x-title x-title :y-title y-title
			 :x-mapping-type x-mapping-type
			 :y-mapping-type y-mapping-type
			 :simple-plot-style plot-style
			 :x-axis x-axis :y-axis y-axis
			 #| :clear-frame-p nil |# ))) ;; do I need it?
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
		       (x-interval (range (slot-value histogram 'breaks)))
		       (y-interval (combined-range (counts histogram) 0))
		       (x-title "x") (y-title "count")
		       (x-mapping-type 'linear-mapping)
		       (y-mapping-type 'linear-mapping)
		       (x-axis t)
		       (y-axis t)
		       (simple-plot-style *default-simple-plot-style*)
		       (line-style nil)
                       vertical-lines-p
		       (fill-color +grey70+))
  "Plot the histogram on frame, return the resulting drawing area.
For the meaning of parameters, see draw-histogram."
  ;; create plot
  (with-slots (breaks counts) histogram
    (let ((drawing-area (plot-simple frame 
				     x-interval
				     y-interval 
				     :x-title x-title :y-title y-title
				     :x-mapping-type x-mapping-type
				     :y-mapping-type y-mapping-type
				     :simple-plot-style simple-plot-style
				     :x-axis x-axis :y-axis y-axis)))
      (draw-histogram drawing-area histogram
		      :line-style line-style
		      :vertical-lines-p vertical-lines-p :fill-color fill-color)
      ;; return drawing-area
      drawing-area)))

(defun plot-columns (frame x ys styles
                     &key (x-interval (range x))
                     (y-interval (range ys))
                     (x-title "")
                     (y-title ""))
  "Plot columns of a matrix."
  (plot-rows frame x (transpose ys) styles
             :x-interval x-interval :y-interval y-interval 
             :x-title x-title :y-title y-title))

(defun plot-rows (frame x ys styles
                  &key (x-interval (range x))
                  (y-interval (range ys))
                  (x-title "")
                  (y-title ""))
  "Plot rows of a matrix YS, matching elements in rows to X.  Styles is either a
sequence conforming to the number of rows, or an atom, which will be replicated
accordingly."
  (bind ((da (plot-simple frame x-interval y-interval
                          :x-title x-title
                          :y-title y-title))
         ((nrow nil) (array-dimensions ys)))
    (if (typep styles 'sequence)
        (assert (= nrow (length styles)))
        (setf styles (make-array nrow :initial-element styles)))
    (iter
      (for row :from 0 :below nrow)
      (for y := (sub ys row t))
      (for line-style :in-sequence styles)
      (draw-lines da x y line-style))
    da))

(defun draw-errorbar (da x y &optional (errorbar-style *default-errorbar-style*))
  "Draw an error bar.  One coordinate is a number, the other should be a vector
of five numbers, which will be drawn as quantiles."
  (bind (((:slots x-mapping y-mapping) da)
         ((:slots thin-line thick-line circle-color circle-size) errorbar-style))
    (cond
      ((and (numberp x) (typep y '(vector * 5)))
       (bind ((#(a1 b1 c b2 a2) y))
         (draw-line da x a1 x b1 thin-line)
         (draw-line da x b1 x b2 thick-line)
         (draw-line da x b2 x a2 thin-line)
         (draw-circle da x c circle-size :fill-color circle-color)))
      ((and (typep x '(vector * 5)) (numberp y))
       (bind ((#(a1 b1 c b2 a2) x))
         (draw-line da a1 y b1 y thin-line)
         (draw-line da b1 y b2 y thick-line)
         (draw-line da b2 y a2 y thin-line)
         (draw-circle da c y circle-size :fill-color circle-color)))
      (t (error "~A and ~A are not recognized as a coordinate and quantile ~
      specification in either order." x y)))))

(defun plot-errorbars (frame matrix names &key (x-interval (range matrix))
                       (x-title "") (divx 50) (gap 20)
                       vertical-line)
  "Plot error bars, which are in matrix rows."
  (clear frame)
  (bind (((nrow ncol) (array-dimensions matrix))
         (names (coerce names 'vector))
         (#(nil right-frame) (split-frame-horizontally frame divx))
         (da (plot-simple right-frame x-interval (make-interval 0 nrow)
                          :x-title x-title :y-axis nil))
         ((:accessors-r/o y-mapping context) da)
         (left (- (interval-left (horizontal-interval da)) gap)))
    (assert (= (length names) nrow))
    (assert (= 5 ncol))
    (iter
      (for row :from 0 :below nrow)
      (for name :in-vector names)
      (draw-errorbar da (sub matrix row t) (1+ row))
      (aligned-text left (map-coordinate y-mapping (1+ row))
                    name :x-align 1
                    :context context))
    (when vertical-line
      (draw-vertical-line da vertical-line +line-dot+))))

