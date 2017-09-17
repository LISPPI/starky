(in-package #:starky)
;;==============================================================================
;;
 (defmacro with-matrix ((newvar &optional initial-contents) &body body)
  "Create a context with a newly initialized matrix, while preserving the 
old one.  The old one is stored in the 9 floats just above the new one.
When the body executes, the new matrix data is available, and active,
while the old matrix data is preserved and restored on exit."
  (let ((init-con (gensym )))
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


(defun stroke-width (width)
  (vg:set-f vg:stroke-line-width width)
  (vg:set-i VG:stroke-cap-style vg:cap-butt)
  (vg:set-i VG:stroke-join-style vg:join-miter))

(defun set-stop (paint stops n)
  (vg:set-parameter-i paint vg:paint-color-ramp-spread-mode
		      vg:color-ramp-spread-reflect)
  (vg:set-parameter-i paint vg:paint-color-ramp-premultiplied 0)
  (vg:set-parameter-fv paint vg:paint-color-ramp-stops (* 5 n)
		       stops)
  (vg:set-paint paint vg:fill-path))

(defun clip-rect (xywh)
  (vg:set-i vg:scissoring 1)
  (vg:set-iv vg:scissor-rects 4 xywh))

(defun clip-end ()
  (vg:set-i vg:scissoring 0))



;;------------------------------------------------------------------------------
(defun new-path ()
  (vg:create-path vg:path-format-standard
		  vg:path-datatype-f
		  1.0 0.0
		  0 0
		  vg:path-capability-append-to))

(defun make-curve (segments coords flags)
  (let ((path (new-path))) 
    (vg:append-path-data path 2 segments coords)
    (vg:draw-path path flags)))

(defun poly (points n flag)
  (let ((path (new-path)))
    (vgu:polygon path points n 0)
    (vg:draw-path path flag)))

(defun polygon (points n)
  (poly points n vg:fill-path))

(defun polyline (points n)
  (poly points n vg:stroke-path))

(defun rect (x y w h)
  (let ((path (new-path)))
    (vgu:rect path x y w h)
    (vg:draw-path path (logior vg:fill-path vg:stroke-path))))

(defun line (x1 y1 x2 y2)
  (let ((path (new-path)))
    (vgu:line path x1 y1 x2 y2)
    (vg:draw-path path  vg:stroke-path)))

(defun round-rect (x y w h rw rh)
  (let ((path (new-path)))
    (vgu:round-rect path x y w h rw rh)
    (vg:draw-path path  (logior vg:fill-path vg:stroke-path))))

(defun ellipse (x y w h)
  (let ((path (new-path)))
    (vgu:ellipse path x y w h)
    (vg:draw-path path  (logior vg:fill-path vg:stroke-path))
    ))

(defun circle (x y r)
  (ellipse x y r r ))

(defun arc (x y w h sa aext)
 (let ((path (new-path)))
    (vgu:arc path x y w h sa aext vgu:arc-open)
    (vg:draw-path path  (logior vg:fill-path vg:stroke-path))))


;;-----------------------------------------------------------------
;; Functions that take a colorspec (any parseable rgb value) wrap
;; themselves in this...
;; one already in the foreign memory space.  Free temps.
#||(defmacro colorspec-function (function colorspec)
  (typecase colorspec
    (integer
     `(let ((colorspec (rgba ,colorspec)))
	(,function colorspec)
	(foreign-free colorspec)))
    (cons
     `(let ((colorspec (rgba ,@colorspec)))
	(,function colorspec)
	(foreign-free colorspec) ))
    (null
     `(,function ,colorspec))
    (t
     `(,function ,colorspec))))
||#
;;-----------------------------------------------------------------
(defun background (foreign-rgba)
  (vg:set-fv vg:CLEAR-COLOR 4 foreign-rgba)
  (vg:clear 0 0 1920 1080))


;;-----------------------------------------------------------------
(defun fill (rgba)
  "set fill to solid rgba paint.  RGBA is a foreign array of floats"
  (let ((paint (vg:create-paint)))
    (vg:set-parameter-i paint vg:PAINT-TYPE vg:PAINT-TYPE-COLOR )
    (vg:set-parameter-fv paint vg:PAINT-COLOR 4 rgba)
    (vg:set-paint paint vg:FILL-PATH)))


(defun fill-linear-gradient (x1 y1 x2 y2 stops cnt)
  (let ((paint (vg:create-paint)))
    (vg:set-parameter-i paint vg:PAINT-TYPE vg:PAINT-TYPE-LINEAR-GRADIENT )
    (vg:set-parameter-fv paint vg:PAINT-LINEAR-GRADIENT 4
			 { :float x1 y1 x2 y2} )
      ;; stops


    (vg:set-parameter-i paint VG:PAINT-COLOR-RAMP-SPREAD-MODE VG:COLOR-RAMP-SPREAD-REFLECT)
    (vg:set-parameter-i paint VG:PAINT-COLOR-RAMP-PREMULTIPLIED 0);multmode


    (vg:set-parameter-fv paint VG:PAINT-COLOR-RAMP-STOPS (* 5 cnt) stops)
    (vg:set-paint paint vg:FILL-PATH)))


(defun fill-radial-gradient (cx cy fx fy r stops cnt)
  (let ((paint (vg:create-paint)))
    (vg:set-parameter-i paint vg:PAINT-TYPE vg:PAINT-TYPE-RADIAL-GRADIENT )
    (vg:set-parameter-fv paint vg:PAINT-RADIAL-GRADIENT 5 { :float cx cy fx fy r })
      ;; stops

    (vg:set-parameter-i paint VG:PAINT-COLOR-RAMP-SPREAD-MODE VG:COLOR-RAMP-SPREAD-REFLECT)
    (vg:set-parameter-i paint VG:PAINT-COLOR-RAMP-PREMULTIPLIED 0);multmode

    (vg:set-parameter-fv paint VG:PAINT-COLOR-RAMP-STOPS (* 5 cnt) stops)
    (vg:set-paint paint vg:FILL-PATH)
    )
  )
;;-----------------------------------------------------------------
(defun stroke (rgba)
  "set stroke to solid rgba paint."
   (let ((paint (vg:create-paint)))
    (vg:set-parameter-i paint vg:PAINT-TYPE vg:PAINT-TYPE-COLOR )
    (vg:set-parameter-fv paint vg:PAINT-COLOR 4 rgba)
    (vg:set-paint paint vg:STROKE-PATH)))


(defun start (w h)
  (let ((white (malloc:rgba #xFFFFFFFF))
	(black (malloc:rgba #x000000FF)))
    (vg:set-fv vg:clear-color 4 white)
    (vg:clear 0 0 w h)
    (fill black)
    (stroke black)
    (stroke-width 1.0 )
    (vg:load-identity)))

(defun end ()
  (let ((err (vg:get-error)))
    (unless (zerop err) (error "end: error ~X" err)))
  
  (egl::errorcheck egl:swap-buffers native::*surface*))
;;-------------------------------------------------------
#||
  Paint is a construct with:
  create   hanles
  destroy
  set      set in context as fill or stroke
  get
  set-color - rgb
  get-color
  pattern - image

VG_PAINT_TYPE (color,linear,radial,pattern)    PARAMETER
VG_PAINT_COLOR (vec4)
VG_PAINT_COLOR_RAMP_SPREAD_MODE
VG_PAINT_COLOR_RAMP_STOPS
VG_PAINT_LINEAR_GRADIENT
VG_PAINT_RADIAL_GRADIENT
VG_PAINT_PATTERN_TILING_MODE

||#


(defun tin1 ()
  (native::init :api egl:openvg-api)
  (vg:set-i vg:rendering-quality vg:quality-better)
  (vg:set-i vg:pixel-layout vg:pixel-layout-rgb-vertical)
  (vg:set-i vg:screen-layout vg:pixel-layout-rgb-vertical)

  ;; testing text buffer prototype
  
  )


(defun tout1 ()
  (native::deinit)
  )
(defun tito ()
  (malloc:with
    (start 1024 768)
    (background (malloc:rgba 1.0 0.3 0.4 1.0))
    (fill (malloc:rgba 1.0 0.5 1.0 1.0))
    (stroke (malloc:rgba 1.0 1.0 0.0 1.0) )

    (circle 512.0 0.0 1024.0)
    (line 0.0 0.0 1024.0 768.0)
    (end)))
