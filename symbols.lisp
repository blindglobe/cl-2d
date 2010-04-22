(in-package :cl-2d)

;;;;  functions for plotting symbols (circles, dots, rectangles, etc)
;;;;
;;;;  The general syntax is
;;;;
;;;;  (symbol-drawing-function x y size color context)
;;;;
;;;;  where x and y are the middle of the symbol, size is the length
;;;;  of an edge othe square that would contain the symbol, which is
;;;;  drawn with color in context.

(defun symbol-filled-circle (x y size color &optional (context *context*))
  "Filled circle symbol."
  (let ((radius (/ size 2)))
    (filled-circle x y radius color context)))

(defun symbol-filled-square (x y size color &optional (context *context*))
  "Filled square symbol."
  (let ((halfsize (/ size 2)))
    (with-context (context)
      (rectangle (- x halfsize) (- y halfsize) size size)
      (set-source-color color)
      (fill-path))))
  
(defun symbol-hollow-circle (x y size color &optional (context *context*))
  "Hollow circle symbol."
  (let ((radius (/ size 2)))
    (with-context (context)
      (circle-path x y radius)
      (set-source-color color)
      (stroke))))

(defun symbol-hollow-square (x y size color &optional (context *context*))
  "Hollow square symbol."
  (let ((halfsize (/ size 2)))
    (with-context (context)
      (rectangle (- x halfsize) (- y halfsize) size size)
      (set-source-color color)
      (stroke))))

;;;;
;;;;  functions that assign a size or a color to a particular weight
;;;;
;;;;  All of them take one argument and return an appropriate color or
;;;;  size.

(defun proportional-size (x)
  "Return a function that multiplies x by the square root of its argument."
  (lambda (s)
    (* x (sqrt s))))
