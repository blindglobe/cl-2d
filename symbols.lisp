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

(defun symbol-filled-circle (x y &key size color (context *context*)
                             &allow-other-keys)
  "Filled circle symbol."
  (let ((radius (/ size 2)))
    (filled-circle x y radius color context)))

(defun symbol-filled-square (x y &key size color (context *context*)
                             &allow-other-keys)
  "Filled square symbol."
  (let ((halfsize (/ size 2)))
    (with-context (context)
      (rectangle (- x halfsize) (- y halfsize) size size)
      (set-source-color color)
      (fill-path))))
  
(defun symbol-hollow-circle (x y &key size color (context *context*)
                             &allow-other-keys)
  "Hollow circle symbol."
  (let ((radius (/ size 2)))
    (with-context (context)
      (circle-path x y radius)
      (set-source-color color)
      (stroke))))

(defun symbol-hollow-square (x y &key size color (context *context*)
                             &allow-other-keys)
  "Hollow square symbol."
  (let ((halfsize (/ size 2)))
    (with-context (context)
      (rectangle (- x halfsize) (- y halfsize) size size)
      (set-source-color color)
      (stroke))))

(defun symbol-label (x y &key size color label (context *context*)
                     &allow-other-keys)
  "Text, with given size."
  (with-context (context)
    (set-source-color color)
    (set-font-size size)
    (aligned-text x y label)))
