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
    (vg:draw-path path  (logior vg:fill-path vg:stroke-path))))

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
      (stroke-width 0 )
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
      ((vec (rgb-back '(0.9 0.2 0.3 1.0)))
       (vec (rgb-fill '(0.0 0.7 1.0 1.0))))
    (background rgb-back)
    (set-fill rgb-fill)
    (circle 500.0 0.0 500.0  )

))

(defun ttt ()
  ( native::init :api egl:openvg-api)
  ;; background
  
  (work)
  ;;  (print (vg:get-error)) (force-output)

  (vg:flush)
  (egl:swap-buffers native::*surface*)
  (sleep 1)
  (native::deinit)
  )

