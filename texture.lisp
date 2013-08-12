
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "load.lisp"))

(defclass <texture> ()
  ((name :initarg :name :reader texture-name)
   (width :initarg :width :reader texture-width)
   (height :initarg :height :reader texture-height)))

(defmacro with-muffle-cffi-bare-warning (&body body)
  `(handler-bind ((alexandria:simple-style-warning
		   #'(lambda (c)
		       (princ "fack")
		       (when (alexandria:starts-with-subseq
			      "bare references to struct types are deprecated."
			      (simple-condition-format-control c))
			 (muffle-warning)))))
     ,@body))

(eval-when (:compile-toplevel)
  (defun load-texture-from-surface (surface)
    (with-muffle-cffi-bare-warning
      (sdl-base::with-pixel (pix (sdl:fp surface))
	(assert (and (= (sdl-base::pixel-pitch pix)
			(* (sdl:width surface) (sdl-base::pixel-bpp pix)))
		     (zerop (rem (sdl-base::pixel-pitch pix) 4))))
	(gl:tex-image-2d :texture-2d 0 :rgba
			 (sdl:width surface) (sdl:height surface)
			 0
			 :bgra :unsigned-byte
			 (sdl-base::pixel-data pix))))))

(defun set-texture-parameter(texture &key
			     (min-filter :linear)
			     (mag-filter :linear)
			     (wrap-s :repeat)
			     (wrap-t :repeat)
			     (border-color '(0 0 0 0)))
  (gl:bind-texture :texture-2d (texture-name texture))
  (gl:tex-parameter :texture-2d :texture-min-filter min-filter)
  (gl:tex-parameter :texture-2d :texture-mag-filter mag-filter)
  (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
  (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
  (gl:tex-parameter :texture-2d :texture-border-color border-color))

(defun create-texture-from-file (fname)
  (let* ((texture-name (car (gl:gen-textures 1)))
	 (surface (sdl-image:load-image fname :image-type :tga))
	 (texture (make-instance '<texture>
				 :name texture-name
				 :width (sdl:width surface)
				 :height (sdl:height surface))))
    (gl:bind-texture :texture-2d texture-name)
    (load-texture-from-surface surface)
    (set-texture-parameter texture)
    texture))
