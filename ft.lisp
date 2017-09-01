(in-package :starky)
;; This is a freetype2 render to bitmap test.
(declaim (optimize (speed 0) (safety 3) (debug 3)))
(defparameter *face*
  (prog1 
      (ft2:new-face "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf")))

(ft2:set-char-size *face* (* 11 64)
		   (* 11 64)  72 72)

(defparameter *cache* (make-hash-table))


;;-----------------------------------------------------------------------------


(defun xdefault-load-render (face char vertical-p)
  "=> BITMAP, ADVANCE, TOP, LEFT
This is the default `LOAD-FUNCTION` for `DO-STRING-RENDER`.  It is also
called in the case that a custom `LOAD-FUNCTION` returns `NIL`, convenient
for caching.
Custom functions must be compatible, though any (non-`NIL`) value may
be returned in the place of `BITMAP`.  Note that cl-freetype2 does nothing
else for you.  If you want your cache populated, you must do this yourself,
for instance, within the [`DO-STRING-RENDER`](#DO-STRING-RENDER) loop."
    (let ((glyphslot (ft2:render-glyph face ;;:lcd
				     )))
  ;;  (ft2:bitmap-convert glypslot  )
    (values (ft2::ft-glyphslot-bitmap glyphslot)
	    (ft2::get-loaded-advance face vertical-p)
            (ft2::ft-glyphslot-bitmap-left glyphslot)
	    (ft2::ft-glyphslot-bitmap-top glyphslot))))

(defun cuck ()
   
  (let ((image (vg:create-image
		vg:s-l-8 700 30
		vg:image-quality-nonantialiased)))

    (ft2:do-string-render (*face* "The \ xxx quick brown fox Jumps over the lazy dog" bm x y)
      (let ((w	 (ft2::ft-bitmap-width bm) )
	    (h	 (ft2::ft-bitmap-rows  bm) ))
	(unless (zerop w)
	  (vg:image-sub-data image
			      (ft2::ft-bitmap-buffer bm)
			     (ft2::ft-bitmap-pitch  bm)
			     vg:a-8; s-l-8
			     x y w h))))

    
    
    (vg:set-i VG:MATRIX-MODE VG:MATRIX-IMAGE-USER-TO-SURFACE) ;
    
    (with-matrix (m (matrix 1.0   0.0  100.0
			    0.0  -1.0  440.0
			    0.0   0.0    1.0))
      (vg:draw-image image))
  
    (vg:set-i VG:MATRIX-MODE VG:MATRIX-PATH-USER-TO-SURFACE) ;
    (vg:destroy-image image)
    nil))

(defun cuck ()
   
  (let ((image (vg:create-image
		vg:s-rgba-8888 700 30
		;;vg:image-quality-better
		vg:image-quality-nonantialiased
		)))

    (ft2:do-string-render (*face* "The xxx \\ quick brown fox Jumps over the lazy dog" bm x y
				  :load-function xdefault-load-render;;:baseline-y-p t
				  )
      (let ((w	    (ft2::ft-bitmap-width  bm))
	    (h	    (ft2::ft-bitmap-rows   bm))
	    (pitch  (ft2::ft-bitmap-pitch  bm))
	    (buffer (ft2::ft-bitmap-buffer bm)))
	(unless (zerop w)
	  (format t "~& ~A "  (ft2::ft-bitmap-pixel-mode bm))

	  (vg:image-sub-data image
;;			     buffer
;;			     pitch
			     (cffi-sys:inc-pointer  buffer (* pitch (-  h 1))) ; last line
			     (- 0 pitch)
			     vg:a-8
			     x   (- 20 h y) w h))))
    ;;	(format t "~&~A ~A ~A ~A ~A error: ~X " bm x y w h  (vg:get-error))
    

    ;;(vg:draw-image image)
    (vg:set-pixels 100 470 image 0  0  700 300)	;
    (vg:destroy-image image)
    nil))


(defun work ()
 ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with:all
      ((vec (rgb-back '(0.0 0.0 0.0 1.0)))		;'(0.9 0.2 0.3 1.0)
       (vec (rgb-fill '(1.0 1.0 1.0 1.0))))
    (background rgb-back)
    (set-fill rgb-fill)
    ;;    (vg:set-i vg:rendering-quality vg:quality-faster)
    (stroke-width 1.0)
    (circle 10.0 10.0 3.0)
    (stroke-width 5.0)
    (circle 500.0 100.0 500.0  )
    (text 100.0 505.0 "The xxx \\ quick brown fox Jumps over the lazy dog" *font* 8.8)
    (cuck)) 
  )
;;(defun work ())

