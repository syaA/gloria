
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :anaphora)
  (ql:quickload :lispbuilder-sdl)
  (ql:quickload :cl-opengl)
  (defun sdl-image-alternative-library (basename)
    (cond
      ((string= basename "libpng12-0.dll") "libpng15-15.dll")
      ((string= basename "jpeg.dll") "libjpeg-8.dll")
      ((string= basename "libtiff-3.dll") "libtiff-5.dll")))
  (handler-bind ((simple-error ;cffi:load-foreign-library-error
		  #'(lambda (c)
		      (use-value (sdl-image-alternative-library
				  (caar (simple-condition-format-arguments c)))))))
    (ql:quickload :lispbuilder-sdl-image)))

(load (compile-file "package.lisp"))

(load "vec.lisp")
(load "util.lisp")
(load (compile-file "texture.lisp"))
(load "sprite.lisp")

