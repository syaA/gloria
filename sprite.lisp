
(defclass <sprite> ()
  ((texture :initarg :texture)
   (base :initarg :base)
   (cell :initarg :cell)
   (rot-center :initarg :rot-center :initform nil)))

(defmethod initialize-after :after ((this <sprite>) &key)
  (with-slots (base rot-center) this
    (unless rot-center
      (setf rot-center base))))

(defun make-sprite-cells (texture cell-width cell-height num-col num-row &optional (init-x 0) (init-y 0))
  (let ((width (texture-width texture))
	(height (texture-height texture)))
    (loop for y from 0 below num-row
       append (loop for x from 0 below num-col
		 collect `(,(/f (+ init-x (* x cell-width)) width)
			    ,(/f (+ init-y (* y cell-height)) height)
			    ,(/f (+ init-x (* x cell-width) cell-width) width)
			    ,(/f (+ init-y (* y cell-height) cell-height) height)
			    ,cell-width
			    ,cell-height)))))
			  
(defun make-sprites (texture base rot-center cells)
  (mapcar #'(lambda (cell) (make-instance '<sprite>
					  :texture texture :base base :rot-center rot-center :cell cell)) cells))

(defun draw-sprite-at (spr x y
		       &key
		       (rot nil rot-sup-p)
		       (w nil w-sup-p)
		       (h nil h-sup-p))
  (with-slots (texture base rot-center cell) spr
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d (texture-name texture))
    (gl:color 1 1 1)
    (destructuring-bind (u0 v0 u1 v1 width height) cell
      (unless w-sup-p (setf w width))
      (unless h-sup-p (setf h height))
      (setf x (- x (vec:x base)))
      (setf y (- y (vec:y base)))
      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (gl:translate x y 0)
      (when rot-sup-p
	(gl:translate (vec:x rot-center) (vec:y rot-center) 0)
	(gl:rotate rot 0 0 1)
	(gl:translate (- (vec:x rot-center)) (- (vec:y rot-center)) 0))
      (gl:with-primitive :quads
	(gl:tex-coord u0 v0)
	(gl:vertex 0 0 0)
	(gl:tex-coord u0 v1)
	(gl:vertex 0 h 0)
	(gl:tex-coord u1 v1)
	(gl:vertex w h 0)
	(gl:tex-coord u1 v0)
	(gl:vertex w 0 0)))))

(defclass <sprite-pattern-animation> ()
  ((pattern :initarg :pattern)
   (wait :initarg :wait :initform 0)
   (loop :initarg :loop :initform nil)))

(defclass <sprite-pattern-animator> ()
  ((animation :initarg :animation :initform nil)
   timer
   (current :initform 0)))

(defun sprite-pattern-animator-reset (animator &optional new-animation)
  (with-slots (animation timer current) animator
    (if new-animation
	(setf animation new-animation))
    (when animation
      (setf current 0)
      (setf timer (slot-value animation 'wait)))))

(defmethod initialize-instance :after ((this <sprite-pattern-animator>) &key)
  (sprite-pattern-animator-reset this))

(defun sprite-pattern-animator-update (animator)
  (with-slots (animation timer current) animator
    (when animation
      (with-slots (pattern wait loop) animation
	(when (<= (decf timer) 0)
	  (when (>= (incf current) (length pattern))
	    (if loop 
		(setf current 0)
		(setf animation nil)))
	  (setf timer wait))))))

(defun sprite-pattern-animator-current (sprite-animator)
  (with-slots (animation current) sprite-animator
    (if animation
	(elt (slot-value animation 'pattern) current))))

