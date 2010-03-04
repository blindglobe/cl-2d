(in-package :cl-2d)

;;;; An interval is a pair of real numbers.  It is not necessarily
;;;; decreasing, as there can be negative intervals (eg for reverse
;;;; plots), but some functions (eg interval-containing and
;;;; interval-intersection) return positive intervals by construction.
;;;; The most important function is interval-of, which returns an
;;;; interval containing its arguments (which can be intervals too,
;;;; among other things), ignoring nil's, or nil if all the arguments
;;;; are nil.  If it encounters a forced-interval, that replaces the
;;;; combination resulting from the previous intervals.

(defclass interval ()
  ((left :initarg :left :accessor left)
   (right :initarg :right :accessor right)))

(defclass forced-interval (interval)
  ()
  (:documentation "When combined using interval-of, replaces the last
  effective union."))

(defun make-interval (left right &optional forced?)
  "Shorthand for creating intervals."
  (make-instance (if forced?
                     'forced-interval
                     'interval)
                 :left left :right right))

(defmethod print-object ((obj interval) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "left=~a  right=~a" (left obj) (right obj))))

(defmethod width ((interval interval))
  "Width of the interval (absolute value)."
  (with-slots (left right) interval
    (abs (- right left))))

(defun interval-diff (interval)
  "Difference between left and right."
  (with-slots (left right) interval
    (- right left)))

(defun interval-midpoint (interval &optional (alpha 0.5))
  "Convex combination of left and right, with alpha (defaults to 0.5)
weight on right."
  (with-slots (left right) interval
    (+ (* (- 1 alpha) left) (* alpha right))))

(defun positive-interval-p (interval)
  "True iff the interval is positive, ie left < right."
  (with-slots (left right) interval
    (< left right)))

(defun negative-interval-p (interval)
  "True iff the interval is negative, ie left > right."
  (with-slots (left right) interval
    (> left right)))

(defun weakly-positive-interval-p (interval)
  "True iff the interval is positive, ie left <= right."
  (with-slots (left right) interval
    (<= left right)))

(defun weakly-negative-interval-p (interval)
  "True iff the interval is negative, ie left >= right."
  (with-slots (left right) interval
    (>= left right)))

(defun zero-interval-p (interval)
  "True iff the left is equal to right."
  (with-slots (left right) interval
    (= left right)))

(defun flip-interval (interval)
  "Exchange left and right."
  (with-slots (left right) interval
    (make-interval right left)))

(defun interval-of (&rest rest)
  "Return a positive interval that encompasses all its arguments,
  which are processed like this:
  - atoms are incorporated, if non-nil
  - lists and vectors are treated as a (flat) sequence of atoms,
    nil's are handled as above
  - intervals are incorporated."
  (let ((left nil)
	(right nil))
    (flet ((process (x)
	     (when x
	       (unless (realp x)
		 (error "arguments have to be real!"))
	       (setf left (if left 
			       (min left x)
			       x))
	       (setf right (if right
			       (max right x)
			       x)))))
      (dolist (r rest)
	(typecase r
	  (null)                                      ; do nothing
	  (number (process r))                       ; process number
	  (sequence (map nil #'process r)) ; process elements of seq.
	  (array (map nil #'process (as 'array (flat r))))
          (forced-interval
             (setf left (left r)
                   right (right r)))
	  (interval                    ; process interval
	   (process (left r))
	   (process (right r)))
	  (t (error "argument ~a is not a list, array, real number or
		    interval." r))))
      (if (and left right)
	  (make-interval left right)
	  nil))))

(defun interval-intersection (&rest intervals)
  "Return intersection of intervals, which is always a (weakly) positive
interval.  Intervals which are nil are silently ignored, if all intervals are
nil, nil is returned."
  ;; flip nonpositive intervals
  (let* ((intervals (mapcar (lambda (interval)
			   (if (positive-interval-p interval)
			       interval
			       (flip-interval interval)))
			 (delete nil intervals))))
    (if intervals
	(make-interval (apply #'max (mapcar #'left intervals))
		       (apply #'min (mapcar #'right intervals)))
	nil)))

;;;;  percentages, fractions and spacers - interpreted relative to
;;;;  width or height, depending on contect.  A spacer divides the
;;;;  remaining area in the given proportion, effectively solving a
;;;;  linear equation. Primarily for frame manipulation.

(defclass fraction ()
  ((frac :initarg :frac :type (real 0 1) :reader frac)))

(defmethod initialize-instance :after ((fraction fraction) &key)
  (with-slots (frac) fraction
    (assert (<= 0 frac 1))))

(defmethod print-object ((obj fraction) stream)
  (print-unreadable-object (obj stream :type t)
    (princ (frac obj) stream)))

(defun fraction (x)
  (make-instance 'fraction :frac x))

(defun fractions (&rest xs)
  "Shorthand function that returns a list of fraction objects."
  (mapcar #'fraction xs))

(defun percent (x)
  (assert (<= 0 x 100))
  (make-instance 'fraction :frac (/ x 100)))

(defun percents (&rest xs)
  "Shorthand function that returns a list of fraction objects."
  (mapcar #'percent xs))

(defclass spacer ()
  ((coefficient :initarg :coefficient :reader coefficient)))

(defmethod print-object ((obj spacer) stream)
  (print-unreadable-object (obj stream :type t)
    (princ (coefficient obj) stream)))

(defmethod initialize-instance :after ((spacer spacer) &key)
  (with-slots (coefficient) spacer
    (assert (plusp coefficient))))

(defun spacer (&optional (coefficient 1))
  (make-instance 'spacer :coefficient coefficient))

(defun spacers (&rest xs)
  "Shorthand function that returns a list of spacer objects."
  (mapcar #'spacer xs))

;;;; interval splitting

(defun split-interval (interval subdivisions)
  "Split the interval using the sequence subdivisions, which can be
positive real numbers, fractions (will be interpreted as a fraction of
_total_ width) and spacers.  If there are no spacers and the
subdivisions don't fill up the interval, a spacer will be added to the
end.  Return a vector of subintervals.  If subdivisions is
nil, (vector interval) is returned."
  (unless subdivisions
    (return-from split-interval (vector interval)))
  (let* ((width (width interval))
	 (direction (cond
		      ((positive-interval-p interval) 1)
		      ((negative-interval-p interval) -1)
		      (t (error "interval has to be nonzero"))))
	 (spacers 0)
	 (non-spacers 0)
	 (subdivisions (map 'list (lambda (div)
				    (etypecase div
					;; numbers just passed through
				      ((real 0)
				       (incf non-spacers div)
				       div)
				      ;; fractions are interpreted
				      (fraction
				       (let ((x (* width 
						   (frac div))))
					 (incf non-spacers x)
					 x))
				      ;; spacers are passed through
				      (spacer 
				       (incf spacers 
					     (coefficient div))
				       div)))
			    subdivisions))
	 (rest (- width non-spacers)))
    (when (minusp rest)
      (error "subdivisions exceed the width of the interval"))
    (when (and (zerop spacers) (plusp rest))
      (setf spacers 1
	    subdivisions (nconc subdivisions (list (spacer 1)))))
    (let* ((left (left interval)))
      (map 'vector (lambda (div)
		     (let ((right (+ left (* direction
					     (etypecase div
					       (number div)
					       (spacer (* rest (/ (coefficient div)
								  spacers))))))))
		       (prog1 (make-interval left right)
			 (setf left right))))
	   subdivisions))))

;;;; auxiliary functions
;;;;

(defun calculate-function (function domain number-of-points
			   ignorable-conditions)
  "Calculate function at number-of-points points on domain.  If a
member of ignorable-conditions is encountered, the condition is
handled and the value is nil.  Return (values xs fxs)."
  (assert (>= number-of-points 2))
  (let ((xs (num-sequence :from (left domain)
			  :to (right domain)
			  :length number-of-points))
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
