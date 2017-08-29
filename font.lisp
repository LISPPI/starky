(in-package #:starky)
;;==============================================================================
;; cold font from storage...Turns out it's not worth a binary save.
;;
;; So we load a lispy cfont, and activate it as seen later.
(defparameter *cold-font* nil)
(defstruct cfont
  name
  count height descender
  codes codecnt ;;instructions, indices and counts...
  points pointx
  advances
  charmap)


;;==============================================================================
;; This is the hot font which gets its data from the cold font.
;; We discard everything except what we need to map the glyps.
(defstruct font
  count descender-height font-height
  glyphs ;; active handles to glyphs
  character-map glyph-advances ;auxiliary data
)


(defun load-font-prim (points point-indices instructions instruction-counts adv cmap ng)
  (let ((&instructions (foreign-alloc :unsigned-char :initial-contents instructions))
	(&points       (foreign-alloc :int  :initial-contents points)))
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
		for proposed-index = 0 then (+ proposed-index  instruction-count) ;;use last count
		for instruction-count  across instruction-counts
		for point-index        across point-indices
;;		for instruction-index across instruction-indices ;; deprecated
		  
		for path =  (vg:create-path vg:path-format-standard
					      vg:path-datatype-s-32
					      (/ 1.0 65536.0) 0.0
					      0 0
					      vg:path-capability-append-to)
	;;	for dummy = (format t "~&---. ~A ~A ~A ~X" i instruction-index proposed-index path)		  
		unless (zerop instruction-count)
		:do
		  (vg:append-path-data path instruction-count
				       (cffi-sys:inc-pointer &instructions proposed-index )
				       (cffi-sys:inc-pointer &points (* 8 point-index)))
	
		collect path)))
      (foreign-free &instructions)
      (foreign-free &points))))

(defparameter *font* nil)
(defun unload-font (font)
  (loop for path across (font-glyphs font)
     do (vg:destroy-path path )))

(defun load-font (coldfont)
  (with-slots (name count height descender
		    codes ;;codex deprecated
		    codecnt
		    points pointx advances charmap) coldfont
    (setf *font*
	  (load-font-prim points pointx
		     codes codecnt
		     advances charmap count)))
  nil)


;; first one gets 0 from 0.  Second,14 from 0. etc.
