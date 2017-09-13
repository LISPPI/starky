(in-package :starky)
;; This is a freetype2 render to bitmap test.
(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;==============================================================================
;; Freetype face
;;
;; Due to bad vg design, there is no way to tell if a glyph has been loaded
;; into vg.  We keep a cache (hasthable char->glyph)
(defclass ft-face ()
  ((face :accessor face :initform nil)
   (cache :accessor cache :initform (make-hash-table))
   (vgfont :accessor vgfont :initform nil)
   (height :accessor height :initform nil) ;line-height
   (width  :accessor width  :initform nil) ;;max advance
   )
  )
;;
;; After basic init, we attach face, create vg font handle, and set size.
;;
(defmethod initialize-instance :after
    ((ftf ft-face)  &key file sizex (sizey sizex) (ppix 72) (ppiy 72))
  (with-slots (face vgfont height width) ftf
    (setf face   (ft2:new-face file)
	  vgfont (vg:create-font 0 :anon t)) ;do not track font handle
    (ft2:set-char-size face  sizex sizey ppix ppiy)
    (multiple-value-bind (xppem yppem xscale yscale ascender descender
				line-height maxadvance)
	(ft2:face-metrics face)
      (declare (ignore xppem yppem xscale yscale ascender descender))
      (setf height line-height
	    width maxadvance))
    ))
;;
;;
(defmethod free ((obj ft-face))
  (with-slots (face cache vgfont) obj
    (vg:destroy-font vgfont)
    (setf cache nil
	  face nil
	  vgfont nil)))

(defparameter *face* nil)



;; Some parameters.  TODO: fix escapement to go with font.
(eval-when (:execute :load-toplevel :compile-toplevel)
  (defparameter *glomal* (vg:make-mm))
  (vg:with-mm *glomal*
    (defparameter *origin* { 0.0 0.0 })))
;;------------------------------------------------------------------------------
;; Add a char glyph to vg font.  The bitmap produced by FreetType2 has much
;; dimensional data in it.  
(defun glyph-add (char ft-bitmap advance left top)
  
  ;; (declare (optimize (speed 0) (safety 3) (debug 3)))
  ;; TODO: come back to memory management.  Statics are useful here
  (vg:with-mallocs
    (let* ((code (char-code char))
	   (w      (ft2::ft-bitmap-width  ft-bitmap))
	   (h      (ft2::ft-bitmap-rows   ft-bitmap))
	   (pitch  (ft2::ft-bitmap-pitch  ft-bitmap))
	   (buffer (ft2::ft-bitmap-buffer ft-bitmap))
	   (vgfont (vgfont *face*))
	   (escapement { :float advance 0.0 }  ))
      ;;      (format t "~A ~A ~A ~A~&" char advance w h) (force-output)
      
      (if (zerop h)	  ;bitmap 0 height, space? fake it (TODO: fix)
	  (vg:set-glyph-to-image vgfont code 0 *origin* escapement)
	  ;;-----------------------------------------------------------------------
	  ;; Create an 8-bit alpha-only bitmap to render into
	  (let ((vg-image (vg:create-image vg:a-8 w h
					   vg:image-quality-nonantialiased
					   )))
	    (vg:image-sub-data vg-image buffer pitch vg:a-8  0 0 w h)
	    (vg:set-glyph-to-image vgfont code vg-image
				   { :float  (float (- left))  (float   top ) }
				   escapement))))))

#||	 ;;render upside-down (negative pitch), since vg coords are upside down.
	 ;; The vector below is calculated origin, accounting for invertedness
;; So, we now create an image-based glyph with our bitmap
(vg:image-sub-data
	  vg-image
	  (cffi-sys:inc-pointer  buffer (* pitch  (-  h 1)));; last line...
	  (- 0 pitch)
	  vg:a-8 ;; wtf?
	  0 0 w h)
||#
;;------------------------------------------------------------------------------
;; Check if glyph is present.  If
(defun glyph-assure (ftf char)
  "Assure that the glyph for this character exists. Return vg glyph id"
  (unless (gethash char (cache ftf))
    (let* ((face (face ftf))
	   (unused (ft2:load-char face char));; ** HERE **
	   (glyphslot (ft2::render-glyph face)))
      (declare (ignore unused))
      (glyph-add char
		 (ft2::ft-glyphslot-bitmap glyphslot)
		 (ft2::get-loaded-advance face nil)
		 (ft2::ft-glyphslot-bitmap-left glyphslot)
		 (ft2::ft-glyphslot-bitmap-top glyphslot))
      ;; TODO: ft2 docs all to Done-glyph - not implemented in bindings...
      (setf (gethash char (cache ftf)) t))))


(defun text (string &optional (mode vg:fill-path))
  "print string using font in *face* in requested mode"
  (loop for char across string do
       (glyph-assure *face* char)
       (vg:draw-glyph (vgfont *face*) (char-code char) mode 0 )))

;; a line-mode output

;; a state-machiney way to fill the buffer
;;=============================================================================
(defclass tb ()
  (;; 
   (columns :accessor columns :initform 80 :initarg :columns)
   (rows :accessor rows :initform 24 :initarg :rows)
   (buffer :accessor buffer :initform nil)
   (linex :accessor linex :initform nil)
   (face :accessor face :initform *face*)) ;;TODO: decouple font cache
  )

(defmethod initialize-instance :after ((tb tb)&key)
  (with-slots (columns rows buffer linex) tb
    (setf buffer (cffi:foreign-alloc
		  :uint  :count (* columns rows) :initial-element 32)
	  linex (make-array rows :fill-pointer 0))))

(defparameter *t* (make-instance 'tb))
;;=============================================================================
;; Set new text into the buffer, break lines, generate line offsets.
;; Note: convoluted because cr needs to be included into the line...
(defun tb-set (tb text)
  "set text and initialize internal structures"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (columns rows buffer linex) tb
    (setf (fill-pointer linex) 0)
    (loop 
       for offset fixnum from 0 below  (* 4  columns rows) by 4
       for len fixnum = 0 then (1+ len)
       for c character across text
       for code = (char-code c)
       with wrap = nil
       do
	 (when wrap
	   (vector-push len linex)
	   (setf len 0))
	 (setf (mem-ref buffer :uint offset) code)
	 (setf wrap (or (= len (1- columns))
			(char= c #\newline))))))
;;=============================================================================
(defun tb-render (tb)
  (let ((origin {0.0 16.0})
	(offset 0))
    (with-slots ( buffer face linex) tb
      (loop for len across linex do ;a single line length;
	   (unless (zerop len)
	     (vg:set-fv vg:glyph-origin 2 origin)
	     ;;       (format t "~&~A ~A |~A|" len offset (subseq *text* offset (+ offset len) ) )
	     (vg:draw-glyphs
	      (vgfont face) len
	      (cffi:inc-pointer buffer (* 4 offset))
	      (cffi:null-pointer)
	      (cffi:null-pointer)
	      vg:fill-path 0))
;;	 (format t "~&ERROR ~A" (vg:get-error))
	   (incf offset len)
	   (incf (cffi:mem-ref origin :float 4) 16.0); origin down
	   ))) )

(defun tb-cachefy-glyphs (tb)
  (loop for i from 0 to 127
     do (glyph-assure *face* (code-char i))))

(defun tb-prep (tb)
  (setf (face tb) *face*)
  (tb-cachefy-glyphs tb)
  (tb-set tb *text*)
  (work-matrix)
  )
;;(defun work ())

(defun work-matrix ()
      (vg:set-i vg:matrix-mode   vg:matrix-glyph-user-to-surface)  ;;vg:matrix-fill-paint-to-user
      (vg:load-identity)
      (vg:scale 1.0 -1.0)
      (vg:translate 0.0 -1080.0)

      (vg:set-i vg:matrix-mode   vg:matrix-path-user-to-surface)  ;;vg:matrix-fill-paint-to-user
      (vg:load-identity)
      (vg:scale 1.0 -1.0)
      (vg:translate 0.0 -1080.0)

      (vg:set-i vg:matrix-mode   vg:matrix-fill-paint-to-user)  ;;vg:matrix-fill-paint-to-user
      (vg:load-identity)
      (vg:scale 1.0 -1.0)
      (vg:translate 0.0 -1080.0)

      (vg:set-i vg:matrix-mode   vg:matrix-stroke-paint-to-user)  ;;vg:matrix-fill-paint-to-user
      (vg:load-identity)
      (vg:scale 1.0 -1.0)
      (vg:translate 0.0 -1080.0)
  )
(defun work ()
 ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
;;

      #||      ((vec (rgb-back '(0.0 0.0 0.0 1.0)))	
      (vec (rgb-fill '(1.0 1.0 0.0 1.0)))
      (vec (rgb-stroke	'(0.9 0.2 0.3 1.0))))
      ||#
  (vg:with-mallocs
    (let ((rgb-back   (vg:rgba))
	  (rgb-fill   (vg:rgba 0.3 1.0 0.9))
	  (rgb-stroke (vg:rgba 0.9 0.2 0.3))
;;	  (origin     { 100.0 520.0 })
	  )

      (background rgb-back)
      (fill rgb-fill)
      (stroke rgb-stroke)
      ;;    (vg:set-i vg:rendering-quality vg:quality-faster)
      (stroke-width 1.0)
      (circle 10.0 10.0 3.0)
      (stroke-width 5.0)
      (circle 500.0 100.0 500.0  )
      ;;    (text 100.0 505.0 "The xxx \\ quick brown fox Jumps over the lazy dog" *font* 8.8)
      (vg:set-i vg:image-mode vg:draw-image-stencil)
      (vg:set-fv vg:glyph-origin 2 {0.0 16.0})
    ;;  (text "The xxx \\ quick brown fox Jumps over the lazy dog VV "  )
    ;;  (vg:set-fv vg:glyph-origin 2 {100.0 536.0})
      ;;  (textline)
      (fill rgb-back)
      (fill rgb-fill)
      (frame-render *frame*)

;;      (time (progn	      (text-dump 80  *text*  )      ))
      ))
  ;;    (vg:draw-glyph *vgfont* (char-code #\g) (+ vg:stroke-path vg:fill-path) 0 )
  (vg:handles-free)
  )

(defun tin ()
  (native::init :api egl:openvg-api)
  (setf *face*
	(make-instance 'ft-face
		       :file "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
		       :sizex (* 11 64)
		       :sizey (* 12 64)))
  
  (vg:set-i vg:rendering-quality vg:quality-better)
  (vg:set-i vg:pixel-layout vg:pixel-layout-rgb-vertical)
  (vg:set-i vg:screen-layout vg:pixel-layout-rgb-vertical)

  ;; testing text buffer prototype
  
  )

(defun ttt ()
  ;; background
  (work)
  (print (vg:get-error)) (force-output)
;;  (vg:set-i vg:rendering-quality vg:quality-better)
  (vg:flush)
  (egl:swap-buffers native::*surface*)
  ;;(sleep 10)
  ;;(egl:swap-buffers native::*surface*)
  
  )
(defun tout ()
  (free *face*)
  (native::deinit)
  )

(defparameter *text* "of the current glyph, and applying the necessary positional adjustments (see Section 11.3), taking into account both the escapement values associated with the glyphs as well as the adjustments_x and adjustments_y parameters.
Following the call, the VG_GLYPH_ORIGIN parameter will be updated with the new origin.
The paintModes parameter controls how glyphs are rendered. If paintModes is 0, neither VGImage-based nor VGPath-based glyphs are drawn.  This mode is useful for determining the metrics of the glyph sequence. If paintModes equals VG_FILL_PATH, VG_STROKE_PATH, or VG_FILL_PATH | VG_STROKE_PATH, path-based glyphs are filled, stroked (outlined), or both, respectively, and image-based glyphs are drawn.
When the allowAutoHinting flag is set to VG_FALSE, rendering occurs without hinting. If allowAutoHinting is equal to VG_TRUE, autohinting may be optionally applied to alter the glyph outlines slightly for better rendering
quality. In this case, the escapement values will be adjusted to match the effects of hinting.")



(defclass frame ()
  ((x :accessor x :initform 100.0 :initarg :x)
   (y :accessor y :initform 500.0 :initarg :y)
   (w :accessor w :initform 500.0 :initarg :w)
   (h :accessor h :initform 400.0 :initarg :h)
   (payload :accessor payload :initarg :payload))
  )

(defparameter *frame* (make-instance 'frame :payload *t*))

(defun frame-render (frame)
  (with-slots (x y w h) frame
    (fill  (vg:rgba))
    (stroke-width 1.0)
    (let ((path (new-path)))
      (vgu:&round-rect path x y w h 10.0 20.0)
      (vg:draw-path path  (logior vg:fill-path vg:stroke-path)))

    (vg:set-i vg:matrix-mode   vg:matrix-glyph-user-to-surface)  ;;vg:matrix-fill-paint-to-user
    (with-saved-matrix
      (vg:translate (+ x 5.0) y)
      (fill (rgba #xFFFFFF))
      (tb-render *t*))))


(with-slots (x y w h) *frame*
  (setf x 200.0
	y 100.0
	w 600.0
	h 400.0))


