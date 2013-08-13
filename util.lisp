
(defun round-off (n)
  (floor (+ n 0.5)))

(defun /f (a b)
  (float (/ a b)))

(defun dump-hash (hash)
  (maphash #'(lambda (k v) (format t "~A => ~A~%" k v)) hash))

(defun deg2rad (deg)
  (* (/ deg 180.0) pi))

(defun rad2deg (rad)
  (* (/ rad pi) 180))

(defgeneric lerp (a b r)
  (:documentation "calc linear interpolation (+ a (* (- b a) r))"))

(defmethod lerp (a b r)
  (+ a (* (- b a) r)))

