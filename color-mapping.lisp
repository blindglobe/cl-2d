(in-package :cl-2d)

;;;;  color-mapping
;;;;
;;;;  A color mapping maps a real number to a color.  Mappings are
;;;;  sophisticated, allow for gamma correction, positive/negative
;;;;  distinction, etc.  Used in image plots.

(defclass color-mapping ()
  ((domain :initarg :domain :accessor domain)
   (color-function :initarg :color-function :accessor color-function))
  (:documentation "A color mapping is a function that maps real
numbers in domain to colors.  Used for plotting image maps."))

(defun make-hue-color-mapping (domain &key
			       (lower-color +blue+) (upper-color +red+)
			       (positivep nil) (nil-color +black+)
			       (gamma 1))
  "Define a gradient from lower-color to upper-color, mapping the
domain into this gradient.  positivep gives the direction on the color
wheel.  If (= lower upper), uses lower-color for all colors.  The
function maps nil to nil-color.  Gamma specifies gamma correction."
  (if (zero-interval-p domain)
	(make-instance 'color-mapping
		       :domain domain
		       :color-function (constantly lower-color))
      (bind (((:interval lower upper) domain)
             (lower-hsv (->hsv lower-color))
             (upper-hsv (->hsv upper-color))
             (difference (- upper lower)))
        (make-instance 'color-mapping
                       :domain domain
                       :color-function #'(lambda (x)
                                           (if x
                                               (hsv-combination 
                                                lower-hsv upper-hsv 
                                                (let ((r (/ (- x lower)
                                                            difference)))
                                                  (if (= gamma 1)
							r
                                                      (expt r gamma)))
                                                positivep)
                                               nil-color))))))

(defun make-sign-color-mapping (abs-max &key (gamma 1) (nil-color +black+))
  "Return a color mapping that maps [(- abs-max),0] to blue-green,
[0, abs-max] to green-red, with optional gamma correction and
nil-color for nil values."
  (assert (plusp abs-max))
  (make-instance 'color-mapping
		 :domain (make-interval (- abs-max) abs-max)
		 :color-function 
		 (let ((lower-hsv (->hsv +blue+))
		       (zero-hsv (->hsv +green+))
		       (upper-hsv (->hsv +red+)))
		   (lambda (x)
		     (if x
			 (let ((abs-normalized (expt (/ (abs x) abs-max)
						     gamma)))
			   (unless (<= abs-normalized 1)
			     (error "value ~a is out of abs-max ~a"
				    x abs-max))
			   (cond
			     ((minusp x)
			      (hsv-combination zero-hsv lower-hsv 
					       abs-normalized t))
			     ((plusp x)
			      (hsv-combination zero-hsv upper-hsv 
					       abs-normalized nil))
			     (t zero-hsv)))
			 nil-color)))))

(defun make-monochrome-mapping (domain &key
				(lower-v 1d0) (upper-v 0d0) (gamma 1))
  "Define a monochrome gradient from lower-v to upper-v (0: black, 1:
white), mapping the domain into this gradient.  If (= lower upper),
uses lower-v for all colors.  Gamma specifies gamma correction."
  (flet ((gray (v)
	   (make-instance 'rgb :red v :green v :blue v)))
    (if (zero-interval-p domain)
	(make-instance 'color-mapping
		       :domain domain
		       :color-function (constantly (gray lower-v)))
	(bind (((:interval lower upper) domain)
	       (difference (- upper lower)))
	  (make-instance 'color-mapping
			 :domain domain
			 :color-function #'(lambda (x)
					     (let* ((r (/ (- x lower)
							  difference))
						    (r-gamma 
						     (if (= gamma 1)
							 r
							 (expt r gamma))))
					       (gray 
						(convex-combination
						 lower-v upper-v r-gamma)))))))))
