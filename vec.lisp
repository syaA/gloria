
(in-package :vec)

(defun make (x y &optional (z 0) (w 1))
  (vector x y z w))

(defun x (v) (aref v 0))
(defun y (v) (aref v 1))
(defun z (v) (aref v 2))
(defun w (v) (aref v 3))

(defun map3 (f &rest vs)
  (make
   (apply f (funcall #'mapcar #'x vs))
   (apply f (funcall #'mapcar #'y vs))
   (apply f (funcall #'mapcar #'z vs))))

(defun add (&rest vs) (apply #'map3 #'+ vs))
(defun sub (&rest vs) (apply #'map3 #'- vs))
(defun mul (&rest vs) (apply #'map3 #'* vs))
(defun div (&rest vs) (apply #'map3 #'/ vs))
(defun scale (v f) (map3 #'(lambda (x) (* x f)) v))

(defun dot (a b)
  (+ (* (x a) (x b))
     (* (y a) (y b))
     (* (z a) (z b))))
(defun cross (a b)
  (make (- (* (y a) (z b)) (* (z a) (y b)))
	(- (* (z a) (x b)) (* (x a) (z b)))
	(- (* (x a) (y b)) (* (y a) (z b)))))
(defun lenq (v)
  (dot v v))
(defun len (v)
  (sqrt (lenq v)))

(defmethod lerp (a b r)
  (add a (scale r (sub b a))))

