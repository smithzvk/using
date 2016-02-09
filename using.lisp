
(in-package :using)

(defmacro label-nc ((name args &body fn-body) &body body)
  "Like LABELS but only takes one definition and doesn't close over any
variables in the environment."
  (let ((fn (gensym (symbol-name name)))
        (rest-sym (gensym)))
    `(let ((,fn ,(eval `(lambda ,args ,@fn-body))))
       (labels ((,name (&rest ,rest-sym) (apply ,fn ,rest-sym)))
         ,@body))))

(defmacro using (args &body body)
  "Execute BODY in a context where only the variables within the argument list
and special bindings are in effect."
  (let ((fn (gensym)))
    `(label-nc (,fn ,args ,@body)
       (,fn ,@args))))
