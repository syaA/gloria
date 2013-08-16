
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
