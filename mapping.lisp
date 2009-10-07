(in-package :cl-2d)

(defclass coordinate-mapping ()
  ((domain :reader domain :type interval :initarg :domain
	   :documentation "interval we map from")
   (conversion :reader conversion :documentation "Mapping function on ~
   (value &optional (snap-mode :none)).  If called outside domain,
   behavior is undefined."))
  (:documentation "A mapping is a function that maps a coordinate
  value to a coordinate on a Cairo context.  Mappings are also used in
  axis labeling and creation.  In order to create a mapping, you
  should use make-instance give the range and the domain."))

(defgeneric map-coordinate (mapping coordinate &optional snap-mode)
  (:method (mapping coordinate &optional (snap-mode :none))
    (funcall (conversion mapping) coordinate snap-mode))
  (:documentation "Map coordinate (a real number) to a Cairo
  coordinate using mapping using the specified snap-mode (should
  default to :none for all methods)."))

(defgeneric range (mapping)
  (:method (mapping)
    (with-slots (domain conversion) mapping
      (make-interval (funcall (conversion (left domain)))
		     (funcall (conversion (right domain))))))
  (:documentation "Return range as an interval."))

;;;;  linear mapping
;;;;  
  
(defclass linear-mapping (coordinate-mapping)
  ()
  (:documentation "A mapping for linear axes."))

(defmethod initialize-instance :after ((coordinate-mapping linear-mapping)
				       &key range snap-p)
  (assert (and (typep range 'interval) (not (zero-interval-p range))))
  (with-slots (domain conversion) coordinate-mapping
    (setf conversion
  	  (let* ((multiplier (/ (interval-diff range) (interval-diff domain)))
  		 (constant (- (left range) (* multiplier (left domain)))))
	    (if snap-p
		(lambda (x &optional (snap-mode :none))
		  (snap (+ (* x multiplier) constant) snap-mode))
		(lambda (x &optional (snap-mode :none))
		  (declare (ignore snap-mode))
		  (+ (* x multiplier) constant))))))
  coordinate-mapping)

;;;;  constant mapping
;;;;

(defclass constant-mapping (coordinate-mapping)
  ()
  (:documentation "A mapping to a constant.  Useful when the range is
  zero."))

(defmethod initialize-instance :after ((coordinate-mapping constant-mapping)
				   &key range)
  (assert (typep range 'interval))
  (setf (slot-value coordinate-mapping 'conversion)
	(constantly (/ (+ (left range) (right range)) 2)))
  coordinate-mapping)


;;;;  log mapping
;;;; 

(defclass log-mapping (coordinate-mapping)
  ()
  (:documentation "A mapping for logarithmic axes."))

(defmethod initialize-instance :after ((coordinate-mapping log-mapping)
				       &key range snap-p)
  (assert (and (typep range 'interval) (not (zero-interval-p range))))
  (with-slots (domain conversion) coordinate-mapping
    (assert (positive-interval-p domain))
    (setf conversion
  	  (let* ((log-left (log (left domain)))
		 (log-right (log (right domain)))
		 (multiplier (/ (interval-diff range) (- log-right log-left)))
  		 (constant (- (left range) (* multiplier log-left))))
	    (if snap-p
		(lambda (x &optional (snap-mode :none))
		  (snap (+ (* (log x) multiplier) constant) snap-mode))
		(lambda (x &optional (snap-mode :none))
		  (declare (ignore snap-mode))
		  (+ (* (log x) multiplier) constant))))))
  coordinate-mapping)
