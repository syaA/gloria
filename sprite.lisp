
(load "util.lisp")

(defclass <sprite> ()
  ((surface :initarg :surface)
   (cell :initarg :cell)))

(defun make-sprite-cells (cell-width cell-height num-col num-row &optional (init-x 0) (init-y 0))
  (loop for y from 0 below num-row
     append (loop for x from 0 below num-col
	       collect `(,(+ init-x (* x cell-width))
			  ,(+ init-y (* y cell-height))
			  ,cell-width
			  ,cell-height))))
			  
(defun make-sprites (surface cells)
  (setf (sdl:cells surface) cells)
  (loop for cell from 0 below (length cells)
     collect (make-instance '<sprite> :surface surface :cell cell)))

(defun draw-sprite-at-* (spr x y)
  (with-slots (surface cell) spr
    (sdl:draw-surface-at-* surface (round-off x) (round-off y) :cell cell)))

(defclass <sprite-pattern-animation> ()
  ((pattern :initarg :pattern)
   (wait :initarg :wait :initform 0)))

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
      (with-slots (pattern wait) animation
	(when (<= (decf timer) 0)
	  (when (>= (incf current) (length pattern))
	    (setf current 0))
	  (setf timer wait))))))

(defun sprite-pattern-animator-current (sprite-animator)
  (with-slots (animation current) sprite-animator
    (if animation
	(elt (slot-value animation 'pattern) current))))

