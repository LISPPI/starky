(in-package :starky)
;; This is a freetype2 render to bitmap test.

(defstruct rect x y w h)

;; a frame starts with client are minimized.  Layout recalculates client area
;; and calls client's resize.

;;==============================================================================
(defclass frame ()
  ((rbounds :accessor rbounds )
   (rclient :accessor rclient )
   (payload :accessor payload :initarg :payload)))

(defmethod initialize-instance :after ((frame frame)&key)
  (with-slots (rbounds rclient) frame
    (setf rbounds (make-rect :x 200.0 :y 500.0  :w 500.0 :h 300.0))
    (setf rclient (make-rect :x 0.0 :y 0.0  :w 0.0 :h 0.0))))

(defmethod print-object ((frame frame) out)
  (print-unreadable-object (frame out :type t)
    (with-slots (x y w h) (rbounds frame)
      (format out "(~A ~A) ~A ~A" x y w h ))))

(defparameter frame-margin 6)
;;==============================================================================
(defmethod layout ((frame frame))
  (with-slots (rbounds rclient payload) frame
    (let ((old-w (rect-w rclient))
	  (old-h (rect-h rclient))
	  ;; calculate usable inside area
	  (new-w (- (rect-w rbounds) (* 2 frame-margin)))
	  (new-h (- (rect-h rbounds) (* 2 frame-margin))))
      (setf (rect-x rclient) frame-margin
	    (rect-y rclient) frame-margin
	    (rect-w rclient) new-w
	    (rect-h rclient) new-h)
      (resize payload new-w new-h))))

(defmacro with-saved-matrix (&body body)
  (let ((old (gensym)))
    `(let ((,old (cffi:foreign-alloc :float :count 9)))
       (vg:get-matrix ,old)
       ,@body
       (vg:load-matrix ,old)
       (cffi:foreign-free ,old))))

;; TODO: client are should really be transparent, so that the client can
;; do what he wants, no?
;;
(defmethod render ((frame frame))
  (with-slots (rbounds payload) frame
    (with-slots (x y w h) rbounds
      (fill-linear-gradient 100.0 800.0  100.0 10.0 *stops* 3 )
;;      (fill  (vg:rgba 0.0 0.0 0.0 0.0))
      (stroke-width 0.1)
      (stroke {.53 .93 .84 1.0})
      (let ((path (new-path)))
	(vgu:&round-rect path x y w h 10.0 20.0)
;;	(vgu:&round-rect path x (+ 16 y) w (- h 16) 10.0 20.0)
;;	(stroke {.53 .93 .84 0.5})
	(vgu:&line path (+ x 3) (+ y 16) (+ x w -18) (+ y 16))
	
	(vg:draw-path path  (logior vg:fill-path vg:stroke-path)))
        
      
      (vg:set-i vg:matrix-mode   vg:matrix-glyph-user-to-surface)  ;;vg:matrix-fill-paint-to-user
      (with-saved-matrix
	(vg:translate (+ x 5.0) y)
	(fill (malloc:rgba #xFFFFFFFF))
	(render payload )))))





