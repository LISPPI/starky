(in-package #:starky)
;;==============================================================================
;;
(defstruct font character-map glyph-advances count descender-height font-height
	   glyphs)

(defparameter max-font-paths 500)


(defun load-font (points point-indices instructions instruction-indices instruction-counts adv cmap ng)
  (let ((&instructions (foreign-alloc :unsigned-char :initial-contents instructions))
	(&points       (foreign-alloc :int  :initial-contents points)   ))
    (%print-mem &points ) 
    (prog1
	(make-font
	    :character-map cmap     :glyph-advances adv
	    :count ng               :descender-height 0
	    :font-height 0
	    :glyphs
	    (make-array
	     ng :initial-contents 
	     (loop for i from 0 upto (1-  ng)
		for instruction-count  across instruction-counts
		for point-index        across point-indices
		for instruction-index  across instruction-indices
		  
		for path = (vg:create-path vg:path-format-standard
					   vg:path-datatype-s-32
					   (/ 1.0 65536.0) 0.0
					   0 0
					   vg:path-capability-append-to)
	 
		unless (zerop instruction-count) :do
		  (format t "~&---. ~A ~X ~X" i path ( vg:get-error ))		  
		  (vg:append-path-data path instruction-count
				       (cffi-sys:inc-pointer &instructions instruction-index)
				       (cffi-sys:inc-pointer &points (* 8 point-index)))
	
		collect path)))
      (foreign-free &instructions)
      (foreign-free &points))))

(defun unload-font (font)
  (loop for path across (font-glyphs font)
     do (vg:destroy-path path )))

(defun load-font-dejavu-sans ()
  (load-font dejavu-sans-glyph-points
	     dejavu-sans-glyph-point-indices
	     dejavu-sans-glyph-instructions
	     dejavu-sans-glyph-instruction-indices
	     dejavu-sans-glyph-instruction-counts
	     dejavu-sans-glyph-advances
	     dejavu-sans-character-map
	     dejavu-sans-glyph-count))

(defun load-font-dejavu-sans-mono ()
  (load-font dejavu-sans-mono-glyph-points
	     dejavu-sans-mono-glyph-point-indices
	     dejavu-sans-mono-glyph-instructions
	     dejavu-sans-mono-glyph-instruction-indices
	     dejavu-sans-mono-glyph-instruction-counts
	     dejavu-sans-mono-glyph-advances
	     dejavu-sans-mono-character-map
	     dejavu-sans-mono-glyph-count))

