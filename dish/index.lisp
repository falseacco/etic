(in-package #:index)

(defun start-server (&optional (port 8080))
  (start (make-instance 'easy-acceptor :port port)))

(setf *js-string-delimiter* #\")
(setf (html-mode) :html5)

;; HTML things
(defmacro defpage (description args &body body) ; bad arg list
  `(define-easy-handler ,description ,args
     (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
       (:html :lang "en"
	      (:head
	       (:meta :charset "utf-8"))
	      (:body
	       ,@body)))))

(defmacro HTML-inline (&body body)
  `(with-html-output (*standard-output* nil :indent t)
     ,@body))

(defun scripts/ (&rest scripts)
  (dolist (script scripts)
    (HTML-inline (:script :src script))))

(defpage (index :uri "/") ()
  (:canvas :id "canvas" :width "100" :height "100" :style "border:1px solid #000000;")
  (scripts/ "static/jquery.min.js"
	    "util.js"
	    "resize-canvas.js"
	    "conway.js"))

;;; Give Hunchentoot access to /static/. (For jQuery)
(defparameter *base-dir*
  (asdf:system-source-directory :dish))

(defparameter *static-local-dir*
  (merge-pathnames (make-pathname :directory '(:relative "static")) *base-dir*))

(push (create-folder-dispatcher-and-handler "/static/" *static-local-dir*)
      *dispatch-table*)

;;; Local scripts
(defmacro defjs ((name &key uri) &body body)
  `(define-easy-handler (,name :uri ,uri) ()
     (setf (content-type*) "text/javascript")
     (ps
       ,@body)))

(defjs (util :uri "/util.js")
  (defun print (&optional (message ""))
    (chain console (log message)))

  (defmacro $m (&body body)
    `(chain ($ ,(car body)) ,@(cdr body))))

;; Make the display fit the user's screen. Not built in for some reason.
(defjs (resize-canvas :uri "/resize-canvas.js")
  (defvar *canvas* (aref ($m "#canvas") 0))
  
  (defun resize-canvas-to-screen ()
    (let* ((window ($ window))
	   (width (chain window (width)))
	   (height (chain window (height))))
      (setf (@ *canvas* width) (* width 0.5)
	    (@ *canvas* height) (* height 0.5))))

  ($m document (ready (lambda ()
			(resize-canvas-to-screen))))
  ($m window (on "resize" (lambda ()
			    (resize-canvas-to-screen)))))

;; gol logic
(defjs (conway :uri "/conway.js")
  (defun make-grid (width height)
    (let ((grid ([])))
      (dotimes (y height grid)
	(let ((row ([])))
	  (dotimes (x width)
	    (chain row (unshift 0)))
	  (chain grid (unshift row))))))

  (defun grid-dimensions (grid)
    (list (@ (aref grid 0) length) (@ grid length)))
  
  (defun pos-x (pos)
    (aref pos 0))

  (defun pos-y (pos)
    (aref pos 1))

  (defmacro dogrid ((pos grid &optional result) &body body) ; var names
    (let ((dims (gensym "DIMS-"))
	  (grid-height (gensym "GRID-HEIGHT-"))
	  (grid-width (gensym "GRID-WIDTH-"))
	  (x (gensym "X-"))
	  (y (gensym "Y-")))
      `(let* ((,dims (grid-dimensions ,grid))
	      (,grid-height (pos-y ,dims))
	      (,grid-width (pos-x ,dims)))
	 (dotimes (,y ,grid-height ,result)
	   (dotimes (,x ,grid-width)
	     (let ((,pos ([] ,x ,y)))
	       ,@body))))))
  
  (defun gget (pos grid)
    (aref grid (pos-y pos) (pos-x pos)))

  (defun gset (pos grid value)
    (setf (aref grid (pos-y pos) (pos-x pos)) value))
  
  (defun pix (grid) ; is-safe
    (dotimes (row (@ grid length))
      (print (+ row "| "
		(chain (aref grid row) (to-string) (replace (regex "/,/g") " "))))))

  )

#|
|| actual cell defs:
||  1 -> alive, 0 or undefined -> dead
||  update-single, then update
||  test with: blinker, tub, queen bee
|| pattern files
|#
