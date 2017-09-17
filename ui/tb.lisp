(in-package :starky)
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
;; We always allocate the maxiamum possible backstore for the text grid.
(defmethod initialize-instance :after ((tb tb)&key)
  (with-slots (columns rows buffer linex) tb
    (setf buffer (cffi:foreign-alloc
		  :uint  :count (* columns rows) :initial-element 32)
	  linex (make-array rows :fill-pointer 0))))

(defparameter *t* (make-instance 'tb))
;;=============================================================================
;; Set new text into the buffer, break lines, generate line offsets.
;; Note: convoluted because cr needs to be included into the line...
;;       0 is written at the end of active buffer.
;; this initial calculation writes the buffer
(defun tb-set (tb text)
  "set text and initialize internal structures"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (buffer columns rows) tb
    (loop for c character across text 
       for offset fixnum from 0 below  (* 4  columns rows) by 4
       do  (setf (mem-ref buffer :uint offset) (char-code c))
       finally (setf (mem-ref buffer :uint offset) 0))))

;;=============================================================================
;; resize.  Generally called by frame when size changed.
(defmethod resize ((tb tb) w h)
  ;; convert to columns and rows.
  (let ((c (floor w (width (face tb))))
	(r (floor h (height (face tb)))))
    (setf (rows tb) r
	  (columns tb) c)
    (layout tb)
    ;; force render
    )
  
  )

;;=============================================================================
;; After the first time, layout
(defmethod layout ((tb tb))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (columns rows buffer linex) tb
    (setf (fill-pointer linex) 0); always rewrite linex.
    (loop
       for offset fixnum from 0 below  (* 4  columns rows) by 4
       for len fixnum = 0 then (1+ len)
       with wrap = nil
       for code = (mem-ref buffer :uint offset)
       until (zerop code)
       do
	 (when wrap
	   (vector-push len linex)
	   (setf len 0))
	   
	 (setf wrap (or (= len (1- columns))
			(= 10 code))))))

;;=============================================================================
;;
;; Render the textubffer
;;
(defmethod render ((tb tb))
  (let ((origin {0.0 32.0})
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
  (tb-set tb *text*) ;; set initial text
  (work-matrix)
  (setf *frame*
	(make-instance 'frame :payload *t*))
  (layout *frame*)
;;  (ttt)
  
  )
;;(defun work ())
