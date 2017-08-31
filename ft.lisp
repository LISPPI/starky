(in-package :starky)
(declaim (optimize (speed 0) (safety 3) (debug 3)))
(defparameter *face*
  (prog1 
      (ft2:new-face "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf")))

(ft2:set-char-size *face* (* 12 64) 0  72 72)
#||
(let ((old-matrix (foreign-alloc :float :count 9))
	(matrix (foreign-alloc :float :initial-contents
			       (list -1.0  0.0 0.0
				     0.0 -1.0 0.0
				     0.0  0.0 1.0))))
;;    (vg:get-matrix old-matrix)
)
()
   (foreign-free old-matrix)
      (foreign-free matrix)

||#
(defmacro with-matrix ((newvar &optional initial-contents) &body body)
  "Create a context with a newly initialized matrix, while preserving the 
old one.  The old one is stored in the 9 floats just above the new one.
When the body executes, the new matrix data is available, and active,
while the old matrix data is preserved and restored on exit."
  (let ((init-con (gensym)))
    `(let* ((,init-con ,initial-contents)
	    (,newvar (foreign-alloc :float
				   :initial-contents ,init-con
				   :count 18)))
       (vg:get-matrix (cffi-sys:inc-pointer ,newvar 36)) ; preserve old matrix.
       (if ,init-con
	   ;; if there is a new matrix, load it now.
	   (vg:load-matrix ,newvar)
	   ;; otherwise, get a copy of the one currently loaded.
	   (vg:get-matrix  ,newvar)) ; and store it as current.
       ,@body
       (vg:load-matrix (cffi-sys:inc-pointer ,newvar 36)); restore old matrix.
       (foreign-free ,newvar))))

;; Reorder the matrix components so they look natural.
;; a b c             a d g
;; d e f  bug VG is  b e h
;; g h i             c f i

(defmacro matrix (a b c
		  d e f
		  g h i)
  `'(,a ,d ,g
     ,b ,e ,h
     ,c ,f ,i))
(defun cuck ()
   
  (let ((image (vg:create-image
		vg:s-l-8 700 30
		vg:image-quality-nonantialiased)))

    (ft2:do-string-render (*face* "The xxx quick brown fox Jumps over the lazy dog" bm x y)
      (let ((w	 (ft2::ft-bitmap-width bm) )
	    (h	 (ft2::ft-bitmap-rows  bm) ))
	(unless (zerop w)
	  (vg:image-sub-data image
			      (ft2::ft-bitmap-buffer bm)
			     (ft2::ft-bitmap-pitch  bm)
			     vg:s-l-8
			     x y w h))))
    ;;	(format t "~&~A ~A ~A ~A ~A error: ~X " bm x y w h  (vg:get-error))
    
    
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
		vg:s-l-8 700 30
		vg:image-quality-nonantialiased)))

    (ft2:do-string-render (*face* "The xxx quick brown fox Jumps over the lazy dog" bm x y
				  ;;:baseline-y-p t
				  )
      (let ((w	    (ft2::ft-bitmap-width  bm))
	    (h	    (ft2::ft-bitmap-rows   bm))
	    (pitch  (ft2::ft-bitmap-pitch  bm))
	    (buffer (ft2::ft-bitmap-buffer bm)))
	(unless (zerop w)
;;	  (format t "~& ~A ~A" h y)

	  (vg:image-sub-data image
;;			     buffer
;;			     pitch
			     (cffi-sys:inc-pointer  buffer (* pitch (-  h 1))) ; last line
			     (- 0 pitch)
			     vg:s-l-8
			     x   (- 20 h y) w h))))
    ;;	(format t "~&~A ~A ~A ~A ~A error: ~X " bm x y w h  (vg:get-error))
    
    
    (vg:draw-image image)
  
    (vg:destroy-image image)
    nil))


(defun work ()
 ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with:all
      ((vec (rgb-back '(1.0 1.0 1.0 1.0)))		;'(0.9 0.2 0.3 1.0)
       (vec (rgb-fill '(0.0 0.0 0.0 1.0))))
    (background rgb-back)
    (set-fill rgb-fill)
    ;;    (vg:set-i vg:rendering-quality vg:quality-faster)
    (stroke-width 1.0)
    (circle 10.0 10.0 3.0)
    (stroke-width 5.0)
    (circle 500.0 100.0 500.0  )
    (text 100.0 500.0 "The qiuick brown fox jumps over the lazy fox" *font* 10.0)
    (cuck)) 
  )
;;(defun work ())

