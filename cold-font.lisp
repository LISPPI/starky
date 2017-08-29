(in-package #:starky)
;;==============================================================================
;; cold font for dealing with font conversions...
;;
;; some observations, such as most bits used, below.
;;
(defparameter *cold-font* nil)
(defstruct cfont
  name count height descender ;;simple fields               Destination
  ;; path instruction database                              run-time
  ;;      type bits  
  codes ;; :U8  4     glyph instructions,                   foreign-ram
  codex ;; ;U16 16  indexes of glyph codes (redundant)
  codecnt ;;U8  6  length of code runs 
  ;; point database
  points   ;S32 x2  x-y pairs consumed by glyph commands    foreign-ram
  pointx   ;U32 16  indexes into points (* 8 for offset)
  advances ;U32 8   width table                              liap
  charmap  ;S16 9   map character code to glyph; -1 is none  lisp
  )

(defun cfont-save (cfont path)
  (with-slots (name count height descender
		    codes codex codecnt
		    points pointx advances charmap) cfont 
    (cl-binary:with-open-binary-file (out path :direction :output)
      (cl-binary:write-u32 out count)
      (cl-binary:write-u32 out height)
      (cl-binary:write-s32 out descender)
      ;; codes
      (cl-binary:write-u32 out (length codes))
      (cl-binary:write-u8vector out codes) ;guaranteed
      (cl-binary:write-u32 out (length codecnt))
      (cl-binary:write-u8vector out codecnt);almost guaranteed
      ;; points
      (cl-binary:write-u32 out (length points))
      (cl-binary:write-s32vector out points)
      (cl-binary:write-u32 out (length pointx))
      (cl-binary:write-u32vector out pointx)
      ;; rest
      (cl-binary:write-u32 out (length advances))
      (cl-binary:write-u32vector out advances)
      (cl-binary:write-u32 out (length charmap))
      (cl-binary:write-s32vector out points)

      
      

      )))
#
