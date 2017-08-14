(defpackage :soba
  (:use :cl :xelf))
  ;exports
(in-package :soba)

(defparameter *title* "sokoban")
(defparameter *width*  640)
(defparameter *height* 480)
(defparameter *fps* 30)

(defparameter *unit* 32)
(defun un (&optional (n 1)) (* n *unit*))

(defclass tile (node)
  ((of-type :initform nil :initarg :of-type :accessor of-type) ; maybe has-image and ground-image, for init-after
   (ground  :initform (make-instance 'ground))
   (has     :initform nil)
   (x       :initform 0 :initarg :x :accessor x)
   (y       :initform 0 :initarg :y :accessor y)))

(defmethod initialize-instance :after ((self tile) &key)
  (with-slots (of-type has) self
    (case of-type
      (:wall (setf has (make-instance 'wall)))
      (otherwise nil))))

(defmethod draw ((self tile))
  (with-slots (x y ground has) self
    (labels ((old-draw (node)
	       (with-slots (color image width height) node
		 (if image
		     (draw-image image x y)
		     (draw-box x y width height :color color)))))
      (old-draw ground)
      (when has
	(old-draw has)))))

(defclass square (node)
  ((width  :initform (un) :initarg :width  :accessor width)
   (height :initform (un) :initarg :height :accessor height)))

(defclass ground (square)
  ((color :initform "coral" :initarg :color :accessor color)))

(defclass wall (square)
  ((color :initform "red" :initarg :color :accessor color)))

(setf test (let ((test (make-thash)))
	     (set-tile (make-instance 'tile :of-type :wall) test)
	     (set-tile (make-instance 'tile :y 1) test)
	     test))

(defun level (&key (tiles (make-thash)) (bg-color "beige"))
  (configure-screen) ;for now
  (with-session
    (open-project :soba)
    (index-pending-resources)
    ; why is this let necessary? Would prefer to just pass level directly <-- maybe once level is a subfunc
    (let ((level (make-instance 'level :tiles tiles :bg-color bg-color)))
      (switch-to-buffer level)
      ;(paste-from level (add-tiles (tiles level)))
      (insert (make-instance 'tile) (un 3) (un 6)))))

(defun add-tiles (thash)
  (with-new-buffer
    (loop for v being each hash-value in thash ; problem here?
       do (insert v))
    (current-buffer)))

(defclass level (buffer)
  ((tiles  :initform (make-thash) :initarg :tiles :accessor tiles)
   (width  :initform *width*)
   (height :initform *height*)
   (background-color :initform "beige" :initarg :bg-color)))

(defun make-thash () (make-hash-table))

(defun tkey (tile &optional nums?)
  (if nums?
      (cons tile nums?)
      (cons (x tile) (y tile))))

(defun set-tile (tile thash)
  (setf (gethash (tkey tile) thash) tile))

(defun rem-tile (tile thash)
  (remhash (tkey tile) thash))

(defun empty-tile (tile thash) ; add ground-img and has-img later
  (with-slots (x y ground) tile
    (set-tile (make-instance 'tile :x x :y y :ground ground) thash)))

(defun configure-screen ()
  (setf *frame-rate* *fps*)
  (setf *font-texture-scale* 1)
  (setf *font-texture-filter* :linear)
  (setf *window-title* *title*)
  (setf *screen-width*  *width*)
  (setf *screen-height* *height*)
  (setf *nominal-screen-width*  *width*)
  (setf *nominal-screen-height* *height*)
  (setf *scale-output-to-window* t))
