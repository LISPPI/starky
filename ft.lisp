(in-package :starky)
;; This is a freetype2 render to bitmap test.
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defclass ft-face ()
  ((face :accessor face :initform nil)
   (cache :accessor cache :initform (make-hash-table))
   (vgfont :accessor vgfont :initform nil)))

(defmethod initialize-instance :after
    ((ftf ft-face)  &key file sizex (sizey sizex) (ppix 72) (ppiy 72))
  (with-slots (face vgfont) ftf
    (setf face   (ft2:new-face file)
	  vgfont (vg:create-font 0))
    (ft2:set-char-size face  sizex sizey ppix ppiy)))

(defmethod free ((obj ft-face))
  (with-slots (face cache vgfont) obj
    (vg:destroy-font vgfont)
    (setf cache nil
	  face nil
	  vgfont nil)))

(defparameter *face* nil)



;; cache of codes we have
(eval-when (:execute :load-toplevel :compile-toplevel)
  (defparameter *glomal* (vg:make-mm))
  (vg:with-mm *glomal*
    (defparameter *origin* #{0.0 0.0})
    (defparameter *escapement* #{7.0 0.0}  )))

(defun glyph-add (char ft-bitmap advance left top)
;; (declare (optimize (speed 0) (safety 3) (debug 3)))
 (let* ((code (char-code char))
	(w      (ft2::ft-bitmap-width  ft-bitmap))
	(h      (ft2::ft-bitmap-rows   ft-bitmap))
	(pitch  (ft2::ft-bitmap-pitch  ft-bitmap))
	(buffer (ft2::ft-bitmap-buffer ft-bitmap))
	(vgfont (vgfont *face*))  )
   (format t "~A ~A ~A ~A~&" char advance w h) (force-output)
 
   (if (zerop h)
       (vg:set-glyph-to-image vgfont code 0 *origin* *escapement*)
       (vg:with-image (vg-image
		       vg:a-8 w h
		       vg:image-quality-nonantialiased)
	 (vg:image-sub-data vg-image
			    (cffi-sys:inc-pointer  buffer (* pitch  (-  h 1))) ; last line
			    (- 0 pitch)
			    vg:a-8
			    0 0 w h)
	 (let ((origin (cffi:foreign-alloc
			:float :initial-contents (list  (float (- left ))
							(float (- h top)))) ))
	   (vg:set-glyph-to-image vgfont code vg-image
				  origin
				  *escapement*)
	   (cffi:foreign-free origin))))))


;; Since OpenVG has no way to query if a glyph is already there,
;; we shall keep a hashtable outside.
  
(defun glyph-assure (ftf char)
  "Assure that the glyph for this character exists. Return vg glyph id"
  (unless (gethash char (cache ftf))
    (setf (gethash char (cache ftf)) t)
    (let* ((face (face ftf))
	   (unused (ft2:load-char face char))
	   (glyphslot (ft2::render-glyph face)))
      (declare (ignore unused))
      (format t "AAA~&") (force-output )
      (glyph-add char
		 (ft2::ft-glyphslot-bitmap glyphslot)
		 (ft2::get-loaded-advance face nil)
		 (ft2::ft-glyphslot-bitmap-left glyphslot)
		 (ft2::ft-glyphslot-bitmap-top glyphslot)))))


(defun text (string mode)
  "print string using font in *face* in requested mode"
  (loop for char across string do
       (glyph-assure *face* char)
       (vg:draw-glyph (vgfont *face*) (char-code char) mode 0 )))
;;-----------------------------------------------------------------------------




(defun work ()
 ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with:all
      ((vec (rgb-back '(0.0 0.0 0.0 1.0)))	
       (vec (rgb-fill '(1.0 1.0 0.0 1.0)))
       (vec (rgb-stroke	'(0.9 0.2 0.3 1.0)
	     )))
    (background rgb-back)
    (fill rgb-fill)
    (stroke rgb-stroke)
    ;;    (vg:set-i vg:rendering-quality vg:quality-faster)
    (stroke-width 1.0)
    (circle 10.0 10.0 3.0)
    (stroke-width 5.0)
    (circle 500.0 100.0 500.0  )
;;    (text 100.0 505.0 "The xxx \\ quick brown fox Jumps over the lazy dog" *font* 8.8)
    (with-vec (origin '(100.0 520.0))
    (vg:set-fv vg:glyph-origin 2 origin))
    (vg:set-i vg:image-mode vg:draw-image-stencil)
    (text "The xxx \\ quick brown fox Jumps over the lazy dog"  vg:fill-path  )
  ;;    (vg:draw-glyph *vgfont* (char-code #\g) (+ vg:stroke-path vg:fill-path) 0 )
) 
  )
;;(defun work ())



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
