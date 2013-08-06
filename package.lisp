
(in-package :common-lisp-user)

(defpackage :prim
  (:use :cl))

(defpackage :vec
  (:use :cl)
  (:export :make
	   :x :y :z :w
	   :add :sub :mul :div :scale
	   :dot :cross
	   :lenq :len))

