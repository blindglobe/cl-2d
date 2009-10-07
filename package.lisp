(defpackage :cl-2d
  (:use :common-lisp 
	:cl-cairo2
	:iterate 
	:cl-colors 
	:cl-utilities 
	:bind
	:cl-numlib
        :xarray)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export

   ;; utilities

   snap snap* segment filled-rectangle circle-path filled-circle
   text-with-extents text x-bearing y-bearing x-advance y-advance
   add-text-extents measure-labels capital-letter-height aligned-text 
   aligned-text-rectangle

   ;; interval

   interval left right make-interval width interval-diff
   positive-interval-p negative-interval-p weakly-positive-interval-p
   weakly-negative-interval-p zero-interval-p flip-interval
   interval-of interval-intersection fraction fractions percent
   percents spacer spacers split-interval calculate-function

   ;; contexts
   
   xlib-image-context-to-png png-context create-png-context

   ;; frame

   frame context background-color width height as-frame padding
   pad-frame split-frame split-frame-vertically
   split-frame-horizontally fill-with-color clear

   ;; mapping

   coordinate-mapping domain conversion map-coordinate range 
   linear-mapping contant-mapping log-mapping

   ;; drawing-area
   
   drawing-area x-mapping y-mapping setup-drawing-area

   ;; styles

   copy-style modify-style font-style name slant weight size color
   dash-style offset dashes line-style width dash-style +dash-solid+
   +dash-dash+ +dash-dot+ +dash-dot-dash+ +line-solid+ +line-dash+
   +line-dot+ +line-dot-dash+ *default-font-style*
   *default-title-font-style* *default-dash-style*
   *default-line-style* *default-left-axis-style* *default-right-axis-style*
   *default-horizontal-axis-style* *default-simple-plot-style*
   *default-image-plot-style* *default-image-legend-style*
   *default-two-sided-plot-style* set-style
   
   ;; axis

   axis standard-axis left-axis top-axis right-axis bottom-axis

   ;; color-mapping
   
   color-mapping make-hue-color-mapping make-sign-color-mapping
   make-monochrome-mapping
 
   ;; legend
   
   horizontal-legend image-legend

   ;; symbols

   symbol-filled-circle symbol-filled-square symbol-hollow-circle
   symbol-hollow-square proportional-size
   
   ;; plot

   draw-line draw-horizontal-line draw-vertical-line draw-lines
   draw-sequence draw-filled-rectangle draw-function draw-histogram
   plot-simple plot-two-sided plot-lines plot-lines-two-sided
   plot-function plot-sequence plot-image plot-histogram
   draw-aligned-text draw-polygon draw-symbol draw-symbols
   
   ))

