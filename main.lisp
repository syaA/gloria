
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "load.lisp"))

(defparameter *assets* (make-hash-table))

(defun register-asset (name content)
  (setf (gethash name *assets*) content))

(defun get-asset (name)
  (gethash name *assets*))

(defun clear-assets ()
  (clrhash *assets*))

(defun asset (symbol-or-object)
  (if (symbolp symbol-or-object)
      (get-asset symbol-or-object)
      symbol-or-object))

(defun register-sprite (name texture
			cell-width cell-height num-col num-row
			&key (init-x 0) (init-y 0) (base-x 0) (base-y 0)
			(rot-center-x 0 rot-center-x-sup-p) (rot-center-y 0 rot-center-y-sup-p))
  (let ((sprs (make-sprites (asset texture)
			    (vec:make base-x base-y 0)
			    (if (or rot-center-x-sup-p rot-center-y-sup-p)
				(vec:make rot-center-x rot-center-y)
				(vec:make base-x base-y))
			    (make-sprite-cells (asset texture)
					       cell-width
					       cell-height
					       num-col
					       num-row
					       init-x
					       init-y))))
    (if (null (cdr sprs))
	(register-asset name (car sprs))
	(loop for spr in sprs
	   for cell from 0
	   do (register-asset (intern (format nil "~A-~A" name cell)) spr)))))

(defun register-sprite-animation (name pattern wait loop)
  (register-asset name
		  (make-instance '<sprite-pattern-animation>
				 :pattern (mapcar #'asset pattern)
				 :wait wait
				 :loop loop)))

(defclass <sprite-animation-set> ()
  ((north :initarg :north)
   (south :initarg :south)
   (east :initarg :east)
   (west :initarg :west)))

(defun register-sprite-animation-set (name north south east west)
  (register-asset name (make-instance '<sprite-animation-set>
				      :north (get-asset north)
				      :south (get-asset south)
				      :east (get-asset east)
				      :west (get-asset west))))

(defun register-key-frame-animation (name key-frame loop)
  (register-asset name
		  (make-instance '<key-frame-animation> :key-frame key-frame :loop loop)))

(defun register-asset-from-file (asset-config-file)
  (let ((base-dir (cadr (pathname-directory asset-config-file))))
    (with-open-file (in asset-config-file)
      (mapc #'(lambda (l) 
		(ecase (car l)
		  (texture (register-asset (cadr l)
					   (create-texture-from-file
					    (make-pathname :directory base-dir :defaults (caddr l)))))
		  (sprite (apply #'register-sprite (cdr l)))
		  (sprite-pattern-animation (apply #'register-sprite-animation (cdr l)))
		  (sprite-animation-set (apply #'register-sprite-animation-set (cdr l)))
		  (key-frame-animation (apply #'register-key-frame-animation (cdr l)))))
	    (read in nil nil)))))


(defparameter *entities* nil)

(defun register-entity (name content)
  (push (cons name content) *entities*))

(defun get-entity (name)
  (anaphora:aif (assoc name *entities*)
       (cdr anaphora:it)))

(defun clear-entities ()
  (setf *entities* nil))

(defun exist-entity? (name)
  (assoc name *entities*))

(defparameter *render-tree* nil)

(defmacro push-render-tree (&body body)
  `(push #'(lambda () ,@body) *render-tree*))

(defun clear-render-tree ()
    (setf *render-tree* nil))

(defgeneric update (obj &key)
  )

(defgeneric draw (obj &key)
  )

(defclass <chara> ()
  ((animation-set :initarg :animation-set)
   animator
   (pos :initarg :pos :initform nil)
   (dir :initarg :dir)
   (movep :initform nil)))

(defmethod initialize-instance :after ((this <chara>) &key)
  (with-slots (animation-set animator dir) this
    (setf animator (make-instance '<sprite-pattern-animator>
				  :animation (slot-value animation-set dir)))))

(defmethod update ((this <chara>) &key)
  (with-slots (animator movep) this
    (if movep
	(sprite-pattern-animator-update animator)))
  t)

(defmethod draw ((this <chara>) &key)
  (with-slots (animator pos) this
    (push-render-tree
     (draw-sprite-at (sprite-pattern-animator-current animator) (vec:x pos) (vec:y pos)))))

(defun chara-change-dir (chara new-dir)
  (with-slots (animation-set animator dir) chara
    (when (not (eq dir new-dir))
      (setf dir new-dir)
      (if dir
	  (sprite-pattern-animator-reset animator (slot-value animation-set dir))))))


(defun register-chara (name animation-set pos dir)
  (register-entity name (make-instance '<chara>
				       :animation-set (asset animation-set)
				       :pos pos
				       :dir dir)))

(defclass <weapon> ()
  ((owner :initarg :owner)
   animator
   (sprite :initarg :sprite)
   (pos :initarg :pos)
   (dir :initarg :dir)))

(defmethod initialize-instance :after ((this <weapon>) &key)
  (with-slots (animator) this
    (setf animator (make-instance '<key-frame-animator> :animation (asset 'weapon-swing)))))

(defmethod update ((this <weapon>) &key)
  (with-slots (owner animator pos) this
    (setf pos (vec:add (slot-value owner 'pos) (vec:make 2 -8 0)))
    (key-frame-animator-update animator)
    (not (key-frame-animator-finish? animator))))

(defmethod draw ((this <weapon>) &key)
  (with-slots (animator sprite pos) this
    (push-render-tree
      (draw-sprite-at sprite (vec:x pos) (vec:y pos) :rot (key-frame-animator-current animator)))))

(defun init ()
  (clear-assets)
  (clear-entities)
  (setf (sdl:frame-rate) 60)
  (register-asset-from-file "assets/assets.lisp")
  (dump-hash *assets*)
  (register-chara 'player 'char1-set (vec:make 100 100 0) 'east))

(defconstant +sqrt2inv+ (/ 1 (sqrt 2)))

(defun dir2vec (dir)
  (case dir
    (north (vec:make 0 -1 0))
    (south (vec:make 0 1 0))
    (west (vec:make -1 0 0))
    (east (vec:make 1 0 0))
    (north-west (vec:make (- +sqrt2inv+) (- +sqrt2inv+) 0))
    (north-east (vec:make +sqrt2inv+ (- +sqrt2inv+) 0))
    (south-west (vec:make (- +sqrt2inv+) +sqrt2inv+ 0))
    (south-east (vec:make +sqrt2inv+ +sqrt2inv+ 0))
    (t (vec:make 0 0 0))))

(defun vel-from-input (n s w e now-dir)
  (when (and n s)
    (setf n nil s nil))
  (when (and w e)
    (setf w nil e nil))
  (cond
    ((and n e) (cons 'north-east (if (eq now-dir 'north) 'north 'east)))
    ((and n w) (cons 'north-west (if (eq now-dir 'north) 'north 'west)))
    ((and s e) (cons 'south-east (if (eq now-dir 'south) 'south 'east)))
    ((and s w) (cons 'south-west (if (eq now-dir 'south) 'south 'west)))
    (n (cons 'north 'north))
    (s (cons 'south 'south))
    (e (cons 'east 'east))
    (w (cons 'west 'west))))

(defun handle-user-input ()
  (let ((player (get-entity 'player)))
    (with-slots (animator pos dir movep) player
      (let ((new-veldir (vel-from-input 
			 (sdl:key-down-p :sdl-key-w)
			 (sdl:key-down-p :sdl-key-s)
			 (sdl:key-down-p :sdl-key-a)
			 (sdl:key-down-p :sdl-key-d)
			 dir)))
	(if new-veldir
	    (progn (setf pos (vec:add pos (vec:scale (dir2vec (car new-veldir)) 3)))
		   (chara-change-dir player (cdr new-veldir))
		   (setf movep t))
	    (progn (sprite-pattern-animator-reset animator)
		   (chara-change-dir player nil)
		   (setf movep nil))))
      (if (sdl:mouse-left-p)
	  (unless (exist-entity? 'weapon)
	    (register-entity 'weapon (make-instance '<weapon>
						    :owner player
						    :pos (slot-value player 'pos)
						    :dir (slot-value player 'dir)
						    :sprite (asset 'sword))))))))

(defun game-update ()
  (handle-user-input)
  (setf *entities*
	(mapcan #'(lambda (acons)
		    (if (update (cdr acons))
			(list acons))) *entities*)))

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defun setup-ortho-projection (width height)
  (setf *screen-width* width)
  (setf *screen-height* height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:viewport 0 0 width height)
  (gl:ortho 0 *screen-width* *screen-height* 0 0 1)
  (gl:matrix-mode :modelview))
  
(defun game-draw ()
  (clear-render-tree)
  (gl:clear :color-buffer-bit)
  (setup-ortho-projection 640 480)
  (mapc #'(lambda (acons)
	       (draw (cdr acons))) *entities*)
  (mapc #'(lambda (drawer) (funcall drawer)) *render-tree*)
  (gl:flush))

(defun game-main ()
    (sdl::with-init ()
      (sdl:window 640 480 :title-caption "Gloria" :flags sdl:sdl-opengl)
      (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
      (init)
      (sdl:with-events ()
	(:quit-event ()
		     (gl:delete-textures (list (get-asset 'char01-sheet-texture)))
		     (clear-assets)
		     (clear-entities)
		     t)
	(:key-down-event ()
			 (when (or (sdl:key-down-p :sdl-key-escape)
				   (sdl:key-down-p :sdl-key-q))
			   (sdl:push-quit-event)))
	(:video-expose-event () (sdl:update-display))
	(:idle ()
	       (game-update)
	       (sdl:clear-display sdl:*black*)
	       (game-draw)
	       (sdl:update-display)))))

