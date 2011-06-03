(defpackage :cl-2d
    (:use :common-lisp 
          :cl-cairo2
          :iterate 
          :cl-colors 
          :cl-utilities 
          :bind
          :cl-num-utils
          :xarray)
  (:shadowing-import-from :iterate :collecting :collect)
  (:shadowing-import-from :cl-num-utils :convex-combination) ; also in cl-colors
  (:shadowing-import-from :cl-cairo2 :mask) ; also in cl-num-utils
  (:export

   ;; utilities

   snap snap* segment filled-rectangle circle-path filled-circle
   text-with-extents text x-bearing y-bearing x-advance y-advance
   add-text-extents measure-labels capital-letter-height aligned-text 
   aligned-text-rectangle

   ;; contexts
   
   xlib-image-context-to-png png-context create-png-context

   ;; frame

   frame horizontal-interval vertical-interval context background-color
   width height as-frame padding pad-frame split-frame split-frame-vertically
   split-frame-horizontally with-clip-to-frame fill-with-color clear

   ;; mapping

   coordinate-mapping domain conversion map-coordinate range 
   linear-mapping contant-mapping log-mapping

   ;; drawing-area
   
   drawing-area x-mapping y-mapping setup-drawing-area x-domain y-domain

   ;; styles

   copy-style modify-style font-style name slant weight size color
   dash-style offset dashes line-style width dash-style +dash-solid+
   +dash-dash+ +dash-dot+ +dash-dot-dash+ +line-solid+ +line-dash+
   +line-dot+ +line-dot-dash+ *default-font-style*
   *default-title-font-style* *default-dash-style*
   *default-line-style* *default-left-axis-style* *default-right-axis-style*
   *default-horizontal-axis-style* *default-simple-plot-style*
   *default-image-plot-style* *default-image-legend-style*
   *default-two-sided-plot-style* errorbar-style *default-errorbar-style* 
   set-style
   
   ;; axis

   axis standard-axis left-axis top-axis right-axis bottom-axis

   ;; color-mapping
   
   color-mapping make-hue-color-mapping make-sign-color-mapping
   make-monochrome-mapping
 
   ;; legend
   
   horizontal-legend image-legend

   ;; symbols

   symbol-filled-circle symbol-filled-square symbol-hollow-circle
   symbol-hollow-square symbol-label
   
   ;; plot

   draw-line draw-regression-line draw-horizontal-line draw-vertical-line
   draw-lines draw-sequence draw-circle draw-rectangle draw-function
   draw-histogram draw-categorical plot-simple plot-two-sided plot-lines
   plot-lines-two-sided plot-symbols plot-function plot-sequence
   plot-image plot-histogram plot-categorical draw-aligned-text
   draw-polygon draw-symbol draw-symbols plot-rows plot-columns
   draw-errorbar plot-errorbars
   
   ))

