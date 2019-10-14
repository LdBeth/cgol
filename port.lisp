(in-package #:cgol)

(defun andf (&rest args)
  "Function version of AND"
  (reduce (lambda (a b) (and a b)) args))

(defmacro putprop (var prop val)
  `(setf (get ,var ,prop) ,val)) 

(defun concat (&rest n)
  (apply 'concatenate 'string n))
