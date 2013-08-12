
(defun round-off (n)
  (floor (+ n 0.5)))

(defun /f (a b)
  (float (/ a b)))

(defun dump-hash (hash)
  (maphash #'(lambda (k v) (format t "~A => ~A~%" k v)) hash))

