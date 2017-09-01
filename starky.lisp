(in-package #:starky)
;;==============================================================================
;;
 

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
(defun text (x y string font pointsize)
  (let ((old-matrix (foreign-alloc :float :count 9))
	(matrix (foreign-alloc :float :initial-contents
			       (list  pointsize 0.0 0.0
				     0.0 pointsize 0.0
				     0.0 0.0 1.0))))
    (vg:get-matrix old-matrix)
    #||
    (loop for q across (font-glyphs font)
       for i from 0 do
	 (format t "~&xxx ~A ~X" i q))
    ||#
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
;;	 (format t "~&B. ~A ~A ~X" xx y( vg:get-error ))
	 (vg:mult-matrix matrix)
;;	 (format t "~&C... ~A ~A ~X" xx y (vg:get-error ))
	 (vg:draw-path (aref (font-glyphs font) glyph-index)
		       vg:fill-path))
;;    (format t "~&D.. glyphindex ~A  ~X ~X"  glyph-index (aref (font-glyphs font) glyph-index)    (vg:get-error ))

       ;;	 (print (char-code c))
	 
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

(defun work ()
 ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with:all
      ((vec (rgb-back '(1.0 1.0 1.0 1.0)		;'(0.9 0.2 0.3 1.0)
	     ))
       (vec (rgb-fill '(0.0 0.0 0.0 1.0))))
    (background rgb-back)
    (set-fill rgb-fill)
    (vg:set-i vg:rendering-quality vg:quality-faster  )
    (circle 500.0 0.0 500.0  )
   ;; (stroke-width 5.0)
    (text 100.0 500.0 "The qiuick brown fox jumps over the lazy fox" *font* 11.0)


))

(defun tin ()
  ( native::init :api egl:openvg-api)
  (load-font *cold-font*)
    (vg:set-i vg:rendering-quality vg:quality-better)
    (vg:set-i vg:pixel-layout vg:pixel-layout-rgb-vertical)
    (vg:set-i vg:screen-layout vg:pixel-layout-rgb-vertical)
    
)
(defun ttt ()
  ;; background
  (work)
  (print (vg:get-error)) (force-output)
  (vg:set-i vg:rendering-quality vg:quality-better
	    )
  (vg:flush)
  (egl:swap-buffers native::*surface*)
  ;;(sleep 10)
  ;;(egl:swap-buffers native::*surface*)
  
  )
(defun tout ()
  (unload-font *font*)
  (native::deinit)
)



;; coordpoint marks a coordinate, preserving a previous color
(defun coordpoint (x y size pcolor)
  (with-vec (color '(0.5 0.0 0.0 0.3))
    (set-fill color)
    (circle x y size)
    (set-fill pcolor)))

(defun grid (x y n w h)
  (with-vec (color '(0.5 0.5 0.5 0.5))
    (set-stroke color)
    (stroke-width 1.0)
    (loop for ix from x to (+ x w) by n
       do (line ix y ix (+ y h)))

    (loop for iy from y to (+ y h) by n
	 do (line x iy (+ x w) iy))))


(defun fuckle (w)
  (with:all
      ((dx:rect (dst-rect 0 0 960 540
					;(+ w 100) 100 920 580
		 ))
       (dx:rect (src-rect 0 0 (ash 1920 16) (ash 1080 16))))
    (let ((update (dx:update-start))
	  (element (mem-ref native::*native* :uint)))
      (dx:element-change-attributes
       update element dx:CHANGE-TRANSFORM 0 255 dst-rect src-rect  0 :transform #x20000)
	(dx:update-submit-sync update)))
  )

;;(loop for i from 10 to 134 by 1 do (fuckle i))

