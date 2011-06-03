(in-package :cl-2d)

(defun horizontal-legend (frame style-text-pairs &optional
			  (horizontal-legend-style
			   *default-horizontal-legend-style*))
  "Using a flat list of styles and texts, draw a legend in frame."
  (with-slots (horizontal-interval vertical-interval context) frame
    (with-context (context)
      (with-slots (font-style left-padding style-left-padding
			      style-text-padding text-right-padding
			      line-length) horizontal-legend-style
	(set-style font-style)
	(let* ((vertical-center (snap* (interval-midpoint vertical-interval)
				       :half))
	       (text-top (- vertical-center 
			    (/ (capital-letter-height) 2)))
	       (horizontal-position (+ (interval-left horizontal-interval) left-padding)))
	  (iter
	    (generate elt :in style-text-pairs)
	    (for style := (next elt))
	    (for text := (next elt))
	    ;; draw the style
	    (incf horizontal-position style-left-padding)
	    (etypecase style
	      (line-style
	       (set-style style)
	       (segment horizontal-position vertical-center
			(+ horizontal-position line-length) vertical-center)
	       (incf horizontal-position (+ line-length style-text-padding))))
	    ;; draw the text
	    (let ((width (aligned-text horizontal-position
				       text-top text
				       :x-align 0 :y-align 0)))
	      (incf horizontal-position (+ width text-right-padding)))))))
    (values)))

(defun image-legend (frame color-mapping &key
		     (image-legend-style *default-image-legend-style*)
		     (z-title ""))
  "Draw an image legend in frame, with given color-mapping and style."
  (with-slots (width number-of-colors padding axis-label-distance)
      image-legend-style
    (bind ((internal-frame (pad-frame frame padding))
	   (#(gradient-frame axis-frame) 
            (split-frame-horizontally internal-frame width (spacer)))
           ((:slots domain color-function) color-mapping)
           ((:structure interval- lower upper) domain))
      (if (zero-interval? domain)
          ;; homogenous rectangle for denegerate 
          (fill-with-color gradient-frame
                           (funcall color-function lower))
          (bind (((:slots-r/o horizontal-interval vertical-interval context)
                  gradient-frame)
                 ((:structure interval- left-edge right-edge) horizontal-interval))
            (with-sync-lock (context)
              (let ((mapping (make-instance 'linear-mapping
                                            :domain domain
                                            :range vertical-interval)))
                ;; draw color gradient
                (iter
                  (for i from 0 below number-of-colors)
                  (for left := (convex-combination 
                                lower upper
                                (/ i number-of-colors)))
                  (for right := (convex-combination 
                                 lower upper
                                 (/ (1+ i) number-of-colors)))
                  (filled-rectangle
                   (funcall color-function 
                            (/ (+ left right) 2))
                   left-edge (map-coordinate mapping left :int)
                   right-edge (map-coordinate mapping right :int)
                   context))
                ;; draw axis
                (right-axis axis-frame mapping t z-title 
                            (slot-value image-legend-style
                                        'axis-style)))))))))
