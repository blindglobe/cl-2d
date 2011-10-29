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

(defmethod range ((mapping coordinate-mapping))
  "Return range of the mapping as an interval."
  (with-slots (domain conversion) mapping
    (interval (funcall conversion (interval-left domain))
              (funcall conversion (interval-right domain)))))

+

;;;;  constant mapping
;;;;

(defclass constant-mapping (coordinate-mapping)
  ()
  (:documentation "A mapping to a constant.  Useful when the range is
  zero."))

(defmethod initialize-instance :after ((coordinate-mapping constant-mapping)
				   &key range snap-p)
  (assert (typep range 'interval))
  (let ((value (interval-midpoint range)))
    (setf (slot-value coordinate-mapping 'conversion)
          (if snap-p
              (lambda (x &optional (snap-mode :none))
                (declare (ignore x))
                (snap value snap-mode))
              (constantly value))))
  coordinate-mapping)

;;;;  linear mapping
;;;;  
  
(defclass linear-mapping (coordinate-mapping)
  ()
  (:documentation "A mapping for linear axes."))

(defmethod initialize-instance :after ((coordinate-mapping linear-mapping)
				       &key range snap-p)
  (assert (and (typep range 'interval) (not (zero-interval? range))))
  (with-slots (domain conversion) coordinate-mapping
    (setf conversion
  	  (let* ((multiplier (/ (interval-diff range) (interval-diff domain)))
  		 (constant (- (interval-left range)
                              (* multiplier (interval-left domain)))))
	    (if snap-p
		(lambda (x &optional (snap-mode :none))
		  (snap (+ (* x multiplier) constant) snap-mode))
		(lambda (x &optional (snap-mode :none))
		  (declare (ignore snap-mode))
		  (+ (* x multiplier) constant))))))
  coordinate-mapping)



;;;;  log mapping
;;;; 

(defclass log-mapping (coordinate-mapping)
  ()
  (:documentation "A mapping for logarithmic axes."))

(defmethod initialize-instance :after ((coordinate-mapping log-mapping)
				       &key range snap-p)
  (assert (and (typep range 'interval) (not (zero-interval? range))))
  (with-slots (domain conversion) coordinate-mapping
    (assert (positive-interval? domain))
    (setf conversion
  	  (let* ((log-left (log (interval-left domain)))
		 (log-right (log (interval-right domain)))
		 (multiplier (/ (interval-diff range) (- log-right log-left)))
  		 (constant (- (interval-left range) (* multiplier log-left))))
	    (if snap-p
		(lambda (x &optional (snap-mode :none))
		  (snap (+ (* (log x) multiplier) constant) snap-mode))
		(lambda (x &optional (snap-mode :none))
		  (declare (ignore snap-mode))
		  (+ (* (log x) multiplier) constant))))))
  coordinate-mapping)
