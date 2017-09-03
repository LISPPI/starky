(in-package #:starky)
;;==============================================================================
;;
 (defmacro with-matrix ((newvar &optional initial-contents) &body body)
  "Create a context with a newly initialized matrix, while preserving the 
old one.  The old one is stored in the 9 floats just above the new one.
When the body executes, the new matrix data is available, and active,
while the old matrix data is preserved and restored on exit."
  (let ((init-con (gensym "fuck")))
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

; VG matrices are in column order, but it row order is easier in text
;; a b c         a d g
;; d e f  VG is  b e h
;; g h i         c f i
(defmacro matrix (a b c
		  d e f
		  g h i)
  `'(,a ,d ,g
     ,b ,e ,h
     ,c ,f ,i))



(defun set-fill (rgba)
  (vg:with-paint (paint)
    (vg:set-parameter-i paint vg:PAINT-TYPE vg:PAINT-TYPE-COLOR )
    (vg:set-parameter-fv paint vg:PAINT-COLOR 4 rgba)
    (vg:set-paint paint vg:FILL-PATH)))

(defun set-stroke (rgba)
   (vg:with-paint (paint)
    (vg:set-parameter-i paint vg:PAINT-TYPE vg:PAINT-TYPE-COLOR )
    (vg:set-parameter-fv paint vg:PAINT-COLOR 4 rgba)
    (vg:set-paint paint vg:STROKE-PATH)))

(defun stroke-width (width)
  (vg:set-f vg:stroke-line-width width)
  (vg:set-i VG:stroke-cap-style vg:cap-butt)
  (vg:set-i VG:stroke-join-style vg:join-miter))

(defun set-stop (paint stops n)
  (vg:set-parameter-i paint vg:paint-color-ramp-spread-mode
		      vg:color-ramp-spread-repeat)
  (vg:set-parameter-i paint vg:paint-color-ramp-premultiplied 0)
  (vg:set-parameter-fv paint vg:paint-color-ramp-stops (* 5 n)
		       stops)
  (vg:set-paint paint vg:fill-path))

(defun fill-linear-gradient (quad stops n)
  (vg:with-paint (paint)
    (vg:set-parameter-i paint vg:PAINT-TYPE vg:paint-type-linear-gradient )
    (vg:set-parameter-fv paint vg:paint-linear-gradient 4 quad)
    (set-stop paint stops n)))

(defun fill-radial-gradient (radial5 stops n)
  (vg:with-paint (paint)
    (vg:set-parameter-i paint vg:PAINT-TYPE vg:paint-type-radial-gradient )
    (vg:set-parameter-fv paint vg:paint-radial-gradient 5 radial5)
    (set-stop paint stops n)))
(defun clip-rect (xywh)
  (vg:set-i vg:scissoring 1)
  (vg:set-iv vg:scissor-rects 4 xywh))

(defun clip-end ()
  (vg:set-i vg:scissoring 0))
;;------------------------------------------------------------------------------
(defun texthack (x y string font pointsize)
  (let ((old-matrix (foreign-alloc :float :count 9))
	(matrix (foreign-alloc :float :initial-contents
			       (list  pointsize 0.0 0.0
				     0.0 pointsize 0.0
				     0.0 0.0 1.0))))
    (vg:get-matrix old-matrix)
    (loop for c across string
       for glyph-index =  (aref  (font-character-map font) (char-code c))
       for xx = x then (+ xx
			  (/ (* pointsize 
				(aref (font-glyph-advances font) glyph-index))
			     65536.0))
       unless (= -1 glyph-index)
       do
	 (setf (mem-ref matrix :float 24) xx
	       (mem-ref matrix :float 28) y  )
	 (vg:load-matrix old-matrix)
	 (vg:mult-matrix matrix)
	 (vg:draw-path (aref (font-glyphs font) glyph-index)
		       vg:fill-path))
	 
    (vg:load-matrix old-matrix)
    (foreign-free old-matrix)
    (foreign-free matrix))
  )


;;------------------------------------------------------------------------------
(defun new-path ()
  (vg:create-path vg:path-format-standard
		  vg:path-datatype-f
		  1.0 0.0
		  0 0
		  vg:path-capability-append-to))

(defun make-curve (segments coords flags)
  (vg:with-path (path (new-path))
    (vg:append-path-data path 2 segments coords)
    (vg:draw-path path flags)))

(defun poly (points n flag)
  (vg:with-path (path (new-path))
    (vgu:polygon path points n 0)
    (vg:draw-path path flag)))

(defun polygon (points n)
  (poly points n vg:fill-path))

(defun polyline (points n)
  (poly points n vg:stroke-path))

(defun rect (x y w h)
  (vg:with-path (path (new-path))
    (vgu:rect path x y w h)
    (vg:draw-path path (logior vg:fill-path vg:stroke-path))))

(defun line (x1 y1 x2 y2)
  (vg:with-path (path (new-path))
    (vgu:line path x1 y1 x2 y2)
    (vg:draw-path path  vg:stroke-path)))

(defun round-rect (x y w h rw rh)
  (vg:with-path (path (new-path))
    (vgu:round-rect path x y w h rw rh)
    (vg:draw-path path  (logior vg:fill-path vg:stroke-path))))

(defun ellipse (x y w h)
  (vg:with-path (path (new-path))
    (vgu:ellipse path x y w h)
    (vg:draw-path path  (logior vg:fill-path vg:stroke-path))
    ))

(defun circle (x y r)
  (ellipse x y r r ))

(defun arc (x y w h sa aext)
  (vg:with-path (path (new-path))
    (vgu:arc path x y w h sa aext vgu:arc-open)
    (vg:draw-path path  (logior vg:fill-path vg:stroke-path))))

(defun start (w h)
  (with-vec (white '(1.0 1.0 1.0 1.0))
    (with-vec (black '(0.0 0.0 0.0 1.0))
      (vg:set-fv vg:clear-color 4 white)
      (vg:clear 0 0 w h)
      (set-fill black)
      (set-stroke black)
      (stroke-width 0.0 )
      (vg:load-identity))))

(defun end ()
  (let ((err (vg:get-error)))
    (unless (zerop err) (error "end: error ~X" err)))
  (egl::errorcheck egl:swap-buffers native::*surface*))

(defun background (rgba)
  (vg:set-fv vg:CLEAR-COLOR 4 rgba)
  (vg:clear 0 0 1200 1080))

