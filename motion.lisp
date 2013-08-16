
; (time value time value ...)
(defclass <key-frame-animation> ()
  ((key-frame :initarg :key-frame)
   (loop :initarg :loop :initform nil)))

(defclass <key-frame-animator> ()
  ((animation :initarg :animation)
   current-frame
   current-value
   (timer :initform 0)))

(defun key-frame-animator-reset (key-frame-animator &optional new-animation)
  (with-slots (animation current-frame current-value timer) key-frame-animator
    (when new-animation
	(setf animation new-animation))
    (with-slots (key-frame) animation
      (setf current-frame key-frame)
      (setf current-value (cadr key-frame))
      (setf timer 0))))

(defmethod initialize-instance :after ((this <key-frame-animator>) &key)
  (key-frame-animator-reset this))

(defun key-frame-animator-update (key-frame-animator)
  (with-slots (animation current-frame current-value timer) key-frame-animator
    (with-slots (key-frame loop) animation
      (incf timer)
      (destructuring-bind (ctime cvalue &optional ntime nvalue &rest rest) current-frame
	(declare (ignore rest))
	(if (null ntime)
	    (when loop
	      (setf current-frame key-frame)
	      (setf current-value (cadr key-frame))
	      (setf timer 0))
	    (if (>= timer ntime)
		(progn
		  (setf current-frame (cddr current-frame))
		  (setf current-value nvalue))
		(setf current-value (lerp cvalue nvalue (/f (- timer ctime) (- ntime ctime))))))))
    current-value))

(defun key-frame-animator-current (key-frame-animator)
  (with-slots (current-value) key-frame-animator
    current-value))

(defun key-frame-animator-finish? (key-frame-animator)
  (with-slots (animation current-frame) key-frame-animator
    (with-slots (loop) animation
      (and (not loop) (null (cddr current-frame))))))

(defclass <rt-animation> ()
  ((rot-animation :initarg :rot-animation :initform nil)
   (trans-animation :initarg :trans-animation :initform nil)))

(defclass <rt-animator> ()
  (rot-animator
   trans-animator))

(defmethod initialize-instance :after ((this <rt-animator>) &key animation)
  (with-slots (rot-animator trans-animator) this
    (with-slots (rot-animation trans-animation) animation
      (if rot-animation
	  (setf rot-animator (make-instance '<key-frame-animator> :animation rot-animation)))
      (if trans-animation
	  (setf trans-animator (make-instance '<key-frame-animator> :animation trans-animation))))))

(defun rt-animator-reset (rt-animator 
			  &key (rt-animation nil rt-animation-sup-p))
  (with-slots (rot-animator trans-animator) rt-animator
    (if rt-animation-sup-p
	(with-slots (rot-animation trans-animation) rt-animation
	  (if rot-animation
	      (key-frame-animator-reset rot-animator rot-animation)
	      (setf rot-animation nil))
	  (if trans-animation
	      (key-frame-animator-reset trans-animator trans-animation)
	      (setf trans-animator nil)))
	(progn
	  (key-frame-animator-reset rot-animator)
	  (key-frame-animator-reset trans-animator)))))

(defun rt-animator-update (rt-animator )
  (with-slots (rot-animator trans-animator) rt-animator
    (if rot-animator
	(key-frame-animator-update rot-animator))
    (if trans-animator
	(key-frame-animator-update trans-animator))))

(defun rt-animator-rot-current (rt-animator)
  (with-slots (rot-animator) rt-animator
    (if rot-animator
	(key-frame-animator-current rot-animator))))

(defun rt-animator-trans-current (rt-animator)
  (with-slots (trans-animator) rt-animator
    (if trans-animator
	(key-frame-animator-current trans-animator))))

(defun rt-animator-finish? (rt-animator)
  (with-slots (rot-animator trans-animator) rt-animator
    (or (and rot-animator (key-frame-animator-finish? rot-animator))
	(and trans-animator (key-frame-animator-finish? trans-animator)))))

