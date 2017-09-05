(in-package #:starky)
;;==============================================================================
;;
 
;;=================================================================
;;

(defun vec-deduce-foreign-type (sequence)
  (if (floatp (elt sequence 0))
      :float
      :int))

(declaim (notinline make-vec))
(defun make-vec (initial-contents
		 &optional type)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type (or null symbol) type))

  (foreign-alloc (or type
		     (vec-deduce-foreign-type initial-contents))
		 :initial-contents initial-contents))

#||
(defmacro with-vec ((var initial-contents &optional type) &body body)
  (let ((ic initial-contents))
    `(let ((,var (typecase ,ic
		   (atom ,ic)
		   ((or vector list)  (make-vec ,ic ,type))
		   (t (error "invalid ~A" (type-of ,ic)))
		   )))
      (unwind-protect
	   (progn (format t "HA~&")
		  ,@body)
	(foreign-free ,var)))))
||#
(defmacro with-vec ((var initial-contents) &body body)
  (let ((ic initial-contents))
    `(let ((,var   (make-vec ,ic )))
       (declare (type cffi-sys:foreign-pointer ,var))
;;      (unwind-protect)
       	  ,@body
      (foreign-free ,var))))

(defmacro with-byte-vec ((var initial-contents) &body body)
  (let ((ic initial-contents))
    `(let ((,var   (foreign-alloc :uint8 :initial-contents ,ic )))
       (declare (type cffi-sys:foreign-pointer ,var))
       ,@body
      (foreign-free ,var))))
;;=================================================================
;; RGBA vecs are often quoted as groups of 0-255 components.

(defun vec-rgba-component (component)
  (typecase component
    (float
     (if (and (<= component 1.0)
	      (>= component 0.0))
	 component
	 (error "RGBA component ~A out of range" component)))
    (integer
     (if (and (<= component 255)
	      (>= component 0))
	 (/ component 255.0)
	 (error "RGBA component ~A out of range" component)))
    (t (error "RGBA component ~A is not integer or float" component))))

;;=================================================================
;; openvg commands are constants, and must be evaluated...

(defun rgba-packed (val)
  (foreign-alloc
   :float
   :initial-contents (list
		     ( / (ldb (byte 8 24) val) 255.0)
		     ( / (ldb (byte 8 16) val) 255.0)
		     ( / (ldb (byte 8 8) val) 255.0)
		     ( / (ldb (byte 8 0) val) 255.0))))
(defun rgba-spy (val)
  (print (list
		     ( / (ldb (byte 8 24) val) 255.0)
		     ( / (ldb (byte 8 16) val) 255.0)
		     ( / (ldb (byte 8 8) val) 255.0)
		     ( / (ldb (byte 8 0) val) 255.0))))

(defun rgba (&rest rest)
  "return a foreign vec of 4 rgb values, given:
- 1 packed rgb integer value, or
- 0, 1, 2, 3 or 4 integer or float values, with defaults."
  (if (and (= 1  (length rest))
	   (integerp (car rest)))
      (rgba-packed (car rest))
      (destructuring-bind (&optional (r 0.0) (g 0.0) (b 0.0) (a 1.0)) rest
	(vg:malloc :float (list r g b a)))))



