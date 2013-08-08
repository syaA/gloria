
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :lispbuilder-sdl))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "load.lisp")
  (load "vec.lisp")
  (load "util.lisp")
  (load "sprite.lisp"))

(defparameter *assets* (make-hash-table))

(defun register-asset (name content)
  (setf (gethash name *assets*) content))

(defun get-asset (name)
  (gethash name *assets*))

(defun clear-assets ()
  (clrhash *assets*))

(defun register-sprite (name surface-name cell-width cell-height num-col num-row &optional (init-x 0) (init-y 0))
  (loop for spr in (make-sprites (get-asset surface-name)
				 (make-sprite-cells cell-width
						    cell-height
						    num-col
						    num-row
						    init-x
						    init-y))
       for cell from 0
       do (register-asset (intern (format nil "~A-~A" name cell)) spr)))

(defun register-sprite-animation (name pattern wait loop)
  (register-asset name
		  (make-instance '<sprite-pattern-animation>
				 :pattern (mapcar #'(lambda (sym) (get-asset sym)) pattern)
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
				      

(defparameter *entities* (make-hash-table))

(defun register-entity (name content)
  (setf (gethash name *entities*) content))

(defun get-entity (name)
  (gethash name *entities*))

(defun clear-entities ()
  (clrhash *entities*))

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
	(sprite-pattern-animator-update animator))))

(defmethod draw ((this <chara>) &key)
  (with-slots (animator pos) this
    (draw-sprite-at-* (sprite-pattern-animator-current animator) (vec:x pos) (vec:y pos))))

(defun chara-change-dir (chara new-dir)
  (with-slots (animation-set animator dir) chara
    (when (not (eq dir new-dir))
      (setf dir new-dir)
      (if dir
	  (sprite-pattern-animator-reset animator (slot-value animation-set dir))))))


(defun register-chara (name animation-set pos dir)
  (register-entity name (make-instance '<chara>
				       :animation-set (get-asset animation-set)
				       :pos pos
				       :dir dir)))

(defun init ()
  (setf (sdl:frame-rate) 60)
  (register-asset 'char01-sheet (sdl:load-image "assets/chara01_a.bmp"
						:color-key-at (sdl:point :x 0 :y 0)))
  (register-sprite 'char01 'char01-sheet 24 32 3 4 72 0)
  (register-sprite-animation 'char01-1-n '(char01-1 char01-2 char01-1 char01-0) 10 t)
  (register-sprite-animation 'char01-1-e '(char01-4 char01-5 char01-4 char01-3) 10 t)
  (register-sprite-animation 'char01-1-s '(char01-7 char01-8 char01-7 char01-6) 10 t)
  (register-sprite-animation 'char01-1-w '(char01-10 char01-11 char01-10 char01-9) 10 t)
  (register-sprite-animation-set 'char01-1 'char01-1-n 'char01-1-s 'char01-1-e 'char01-1-w)
  (register-chara 'player 'char01-1 (vec:make 100 100 0) 'east))

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
	    (progn (setf pos (vec:add pos (dir2vec (car new-veldir))))
		   (chara-change-dir player (cdr new-veldir))
		   (setf movep t))
	    (progn (sprite-pattern-animator-reset animator)
		   (chara-change-dir player nil)
		   (setf movep nil)))))))

(defun game-update ()
  (handle-user-input)
  (maphash #'(lambda (k v) (update v)) *entities*))

(defun game-draw ()
  (maphash #'(lambda (k v) (draw v)) *entities*))

(defun game-main ()
    (sdl::with-init ()
      (sdl:window 600 480 :title-caption "Gloria")
      (init)
      (sdl:with-events ()
	(:quit-event ()
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


;(sdl:draw-surface (sdl-image:load-image "assets/Character Boy.png" :image-type :png))

