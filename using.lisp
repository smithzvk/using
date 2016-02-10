
(in-package :using)

(defmacro flet-nc ((name args &body fn-body) &body body)
  "Like FLET but only takes one definition and doesn't close over any variables
in the environment."
  (let ((fn (gensym (symbol-name name))))
    `(let ((,fn ,(eval `(lambda ,args ,@fn-body))))
       (flet ((,name (&rest rest) (apply ,fn rest)))
         ,@body))))

(defmacro using (args &body body)
  "Execute BODY in a context where only the variables within the argument list
and special bindings are in effect."
  (let ((fn (gensym)))
    `(flet-nc (,fn ,args ,@body)
       (,fn ,@args))))
