(in-package #:wo)

(define-symbol-macro asd
    (asdf:make :wo))

(defmethod print-object ((hash hash-table) stream)
  (format stream "{~%~{~{    ~S => ~S~%~}~}}"
	  (loop for key being the hash-keys in hash
	     using (hash-value value)
	              collect (list key value))))

(defmacro doplist ((var1 var2 alist &optional result) &body body)
  (let ((galist (gensym "GALIST-")))
    `(do* ((,galist ,alist (cddr ,galist))
	   (,var1 (first ,galist) (first ,galist))
	   (,var2 (second ,galist) (second ,galist)))
	  ((null ,galist) ,result)
       ,@body)))

(defun hash (&rest parameters)
  (let ((new (apply #'make-hash-table (cdr parameters))))
    (doplist (key value (car parameters) new)
      (setf (gethash key new) value))))

(defun import-config-from-file (path)
  (let ((config (hash nil :test #'eq)))
    (with-open-file (file path
			  :direction :input)
      (do ((data (read file) (read file nil)))
	  ((null data) config)
	(setf (gethash (car data) config) (hash (cdr data) :test #'eq))))))

(defparameter *unit-defs*
  (import-config-from-file "config/aw1/unit-defs.txt"))

(defparameter *terrain-defs*
  (import-config-from-file "config/aw1/terrain-defs.txt"))

(defparameter *CO-defs*
  (import-config-from-file "config/aw1/CO-defs.txt"))

(defparameter *bdt*
  (import-config-from-file "config/aw1/bdt.txt"))

(defun make-team (id CO &rest other)
  `(,id ,(append (list :CO CO) other)))

(defun teams (&rest teams)
  (apply #'append teams))

(defparameter *teams*
  (teams (make-team 1 :max :is-cop nil) (make-team 2 :kanbei :is-cop nil)))

(defun team-get (property id)
  (getf (getf *teams* id) property))

(flet ((config-get (inner outer def)
	 (gethash inner (gethash outer def))))
  
  (defun unit-get* (property type)
    (config-get property type *unit-defs*))

  (defun terrain-get* (property type)
    (config-get property type *terrain-defs*))

  (defun CO-get (property name)
    (config-get property name *CO-defs*))

  (defun matchup (att def)
    (config-get def att *bdt*)))

(defun make-unit (type team &rest other)
  (append (list :type type
		:team team
		:hp 100
		:fuel (unit-get* :move-fuel type)
		:ammo (unit-get* :wpn1-ammo type))
	  other))

(defun display-hp (unit)
  "Derives the display HP of a unit (1-10)."
  (let ((base-hp (round (/ (unit-get :hp unit) 10))))
    (if (zerop base-hp)
	1
	base-hp)))

(defun unit-get (property unit)
  (case property
    (:CO (team-get :CO (unit-get :team unit)))
    (:display-hp (display-hp unit))
    (t (or (getf unit property) (unit-get* property (getf unit :type))))))

(defun swap (pair)
  (list (second pair) (first pair)))

(defun make-map (dimensions &key initial-contents)
  (if initial-contents
      (make-array (swap dimensions) :initial-contents initial-contents)
      (make-array (swap dimensions))))

(defun import-map (path)
  (with-open-file (f path :direction :input)
    (let* ((format (read f))
	   (dims (read f))
	   (map (make-map dims :initial-contents (read f))))
      `(:format ,format :dims ,dims :map ,map))))

(defparameter *map*
  (getf (import-map "map/blank.txt") :map))

(defun is-in-bounds (pos)
  "Checks if pos is defined for map."
  (let* ((dims (array-dimensions *map*))
	 (height (first dims))
	 (width (second dims))
	 (px (first pos))
	 (py (second pos)))
    (and (>= px 0) (>= py 0) (< px width) (< py height))))

(defun make-tilev (terrain &rest other)
  (append `(,terrain) other))

(defun tile@ (pos)
  "Fetches the tile at a certain map position."
  (when (is-in-bounds pos)
    (apply #'aref *map* (swap pos))))

(defun tile-get (property tile)
  "Fetches a property of a tile."
  (if (eq property :terrain)
      (first tile)
      (getf (cdr tile) property)))

(defun tile@-get (property pos)
  "Fetches a property of a tile at a map position."
    (tile-get property (tile@ pos)))

(defun unit@ (pos)
  (tile@-get :unit pos))

(defun unit@-get (property pos)
  (unit-get property (unit@ pos)))

(defun terrain@ (pos)
  (tile@-get :terrain pos))

(defun terrain@-get (property pos)
  (terrain-get* property (terrain@ pos)))

(defun groom-range (range)
  "Converts input to a form acceptable to fn range."
  (cond ((null range) '(0 0))
	((consp range) range)
	(t `(1 ,(abs range)))))

(defun range (range)
  (let* ((range (groom-range range))
	 (bot (first range))
	 (top (second range)))
    (if (and (<= bot top) (>= bot 1) (>= top 1))
	(loop for n from bot upto top collect n)
	0))) ; Safe default.

(defun von-Neumann (n &optional (origin '(0 0)))
  "Finds the Von-Neumann neighborhood exactly n cells away from the origin."
  (do* ((acc nil)
	(ox (first origin))
	(oy (second origin))

	(y-offset (- n) (1+ y-offset))
	(x-offset 0 (- n (abs y-offset))))
       ((> y-offset n) acc)
    (if (zerop x-offset)
	(push `(,ox ,(+ oy y-offset)) acc)
	(progn
	  (push `(,(+ ox x-offset) ,(+ oy y-offset)) acc)
	  (push `(,(- ox x-offset) ,(+ oy y-offset)) acc)))))

(defun neighbors (pos &optional (mode #'von-Neumann))
  (funcall mode 1 pos))

(defun make-queue (&rest objects)
  "<Ripped from ANSI Common Lisp>"
  (let ((queue (cons nil nil)))
    (dolist (object objects queue)
      (enqueue object queue))))

(defun enqueue (object queue)
  (if (null (car queue))
      (setf (cdr queue) (setf (car queue) (list object)))
      (setf (cdr (cdr queue)) (list object)
	    (cdr queue) (cdr (cdr queue))))
  (car queue))

(defun dequeue (queue)
  (pop (car queue)))

(defun path-find (unit pos)
  "Calculates a path to each tile a unit could possibly move to."
  (do* ((move-type (unit-get :move-type unit))
	(move-range (unit-get :move-range unit))
	(turns (1+ (expt 4 (1- move-range))) (1- turns))

	(frontier (make-queue pos))
	(curr (dequeue frontier) (dequeue frontier))

	(paths (hash `(,pos (:came-from nil :move-cost 0)) :test #'equal)))
       ((or (null curr) (zerop turns)) paths)
    (dolist (next (remove-if (lambda (pos)
			       (or
				;; Algorithm has already visited location.
				(gethash pos paths)
				;; Location is undefined for current map.
				(not (is-in-bounds pos))))
			     (neighbors curr)))
      (let ((new-move-cost (let ((next-cost (terrain@-get move-type next)))
			     (if (null next-cost)
				 99 ; lol
				 (+ next-cost
				    (getf (gethash curr paths) :move-cost))))))
	(cond (;; The unit can move to next pos, and has move left over.
	       (< new-move-cost move-range)
	       (enqueue next frontier)
	       (setf (gethash next paths)
		     `(:came-from ,curr :move-cost ,new-move-cost)))
	      (;; The unit can move to next pos, but can't move further.
	       (= new-move-cost move-range)
	       (setf (gethash next paths)
		     `(:came-from ,curr :move-cost ,new-move-cost)))
	      (;; The unit can't move to next pos.
	       t nil))))))

(defun extrapolate-path (destination paths)
  "Retrieves a path to destination."
  (do ((curr destination (getf (gethash curr paths) :came-from))
       (acc nil))
      ((null curr) acc)
    (push curr acc)))

(defun %range-of-movement (unit pos)
  "<You really don't want to be calling path-find each time this is called.>"
  (loop for pos being each hash-key in (path-find unit pos) collect pos))

(defun %range-of-attack (unit pos)
  "<Don't try to optimize this yet. Also it's wrong>"
  (%range-of-movement unit pos))

(defun range-of (type unit pos)
  (case type
    (:movement (%range-of-movement unit pos))
    (:attack (%range-of-attack unit pos))
    (t (remove-if-not #'is-in-bounds
		      (mapcan (lambda (n)
				(von-Neumann n pos))
			      (range (case type
				       (:wpn1 (unit-get :wpn1-range unit))
				       (:wpn2 (unit-get :wpn2-range unit))
				       (:vision (unit-get :vision unit)))))))))

(defparameter *#-teams*
  (/ (length *teams*) 2))

(defparameter *current-turn*
  1)

;; advance-turn (setf cdr

(defun %base-damage (att def)
  "Fetches raw matchup data from global BDT."
  (gethash (unit-get :type def)
	   (gethash (unit-get :type att) *bdt*)))

(defun base-damage (att def)
  "Fetches an attacking unit's base damage against another."
  (let* ((base (%base-damage att def))
	 (wpn1? (first base))
	 (wpn2 (second base)))
    (if (and wpn1? (> (unit-get :wpn1-ammo att) 0))
	(values wpn1? t)
	(values wpn2 nil))))

(defun calc-hp (unit &key is-counter-attacking)
  "[?] Derives the HP value used for damage calculations."
  (if is-counter-attacking
      (/ 100 (unit-get :hp unit))
      (/ 10 (unit-get :display-hp unit))))

(defun %terrain-cover-* (terrain)
  "[AW1-3] Derives the defense boost given by terrain not accounting for unit HP."
  (* 10 (terrain-get* :stars terrain)))

(defun terrain-cover (terrain def)
  "Derives the defense boost granted to a unit from its terrain."
  (/ (- 100
	(* (%terrain-cover-* terrain)
	   (calc-hp def)))
     100))

(defun cat (type)
  (case type
    ((:infantry :mech) :infantry)
    ((:recon :light-tank :mid-tank :anti-air) :direct)
    ((:artillery :rockets :missiles) :indirect)
    ((:fighter :bomber :battle-copter) :air)
    ((:cruiser :sub) :direct-sea)
    ((:battleship) :indirect-sea)
    (:apc :transport)
    (:transport-copter :transport-air)
    (:lander :transport-sea)
    (t nil)))

(defun check-mods (mods unit)
  (let ((cat (cat (unit-get :type unit)))
	valid)
    (dolist (mod mods valid)
      (when (member (car mod) `(:all ,cat) :test #'eq)
	(push (cdr mod) valid)))))

(defun get-mods (unit type)
  (check-mods (CO-get type (unit-get :CO unit)) unit))

(defun %calculate-unit-modifier (type stat unit)
  (let ((multiplicand 1))
    (dolist (mod (get-mods unit type) multiplicand)
      (when (eq stat (car mod))
	(setf multiplicand (* multiplicand (second mod)))))))

(defun d2d (stat unit)
  (%calculate-unit-modifier :d2d-mods stat unit))

(defun norm (stat unit)
  (%calculate-unit-modifier :norm-mods stat unit))

(defun luck-bonus (&key (floor 0) (ceiling 9))
  "Extra damage granted to attacking units."
  (+ floor (random (- ceiling floor))))

(defun damage (att-pos def-pos)
  "Calculates the damage dealt to the unit at def-pos when attacked by the unit
   at att-pos."
  (let ((attacker (unit@ att-pos))
	(defender (unit@ def-pos)))
    (+ (reduce (lambda (x y)
		 (floor (* x y)))
	       `(,(d2d :attack attacker)
		  ,(base-damage attacker defender)
		  ,(norm :attack attacker)
		  ,(d2d :defense defender)
		  ,(norm :defense defender)
		  ,(calc-hp attacker)
		  ,(terrain-cover (terrain@ def-pos) defender)))
       (luck-bonus))))

(defvar *server* nil)

(defun stop-server ()
  (when *server*
    (stop *server*)
    (setf *server* nil)))

(defun start-server (&optional (port 8080))
  (stop-server)
  (setf *server* (start (make-instance 'easy-acceptor :port port))))

(setf (html-mode) :html5
      *js-string-delimiter* #\")

(define-easy-handler (index :uri "/") ()
  (with-html-output-to-string (s)
    (:html :lang "en"
     (:head
      (:meta :charset "utf-8")
      (:title "wo"))
     (:body
      (:canvas
       :id "main-display"
       :width "640"
       :height "480"
       :style "border:1px solid black;")
      (:script :type "text/javascript"
	       (str (ps
		      (defun greet ()
			(chain console (log "Hello, world~"))))))))))

;; Sprites can p just be a bunch of arrays containing the args to call-w/e
