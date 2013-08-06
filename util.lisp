
(defun round-off (n)
  (floor (+ n 0.5)))

(defun dump-hash (hash)
  (maphash #'(lambda (k v) (format t "~A => ~A~%" k v)) hash))

