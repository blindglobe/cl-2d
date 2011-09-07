;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-2d)

(defparameter *pdf-frame-dimensions* '(400 300))

(defun calculate-width-height (specification)
  (bind (((width &optional (height (ceiling (* 0.75 width))))
          (ensure-list specification)))
    (list width height)))

(defmacro with-pdf-frame ((frame filename 
                           &key (dimensions '*pdf-frame-dimensions*))
                          &body body)
  "Plot into a PDF file, with FRAME bound to the frame."
  (check-type frame symbol)
  (with-unique-names (context dimensions-var)
    `(let* ((,dimensions-var (calculate-width-height ,dimensions))
            (,context (create-pdf-context ,filename
                       (first ,dimensions-var) (second ,dimensions-var)))
            (,frame (as-frame ,context)))
       (unwind-protect
            (progn ,@body)
         (destroy ,context)))))
