
(in-package :using)

(defmacro flet-nc ((name args &body fn-body) &body body)
  "Like FLET but only takes one definition and doesn't close over any variables
in the environment."
  (let ((fn (gensym (symbol-name name))))
    `(let ((,fn ,(eval `(lambda ,args ,@fn-body))))
       (flet ((,name (&rest rest) (apply ,fn rest)))
         ,@body))))

(defvar *label-nc-fn* nil
  "This holds the binding of the label-nc function.")

(defmacro label-nc ((name args &body fn-body) &body body)
  "Like LABELS but only takes one definition and doesn't close over any
variables in the environment."
  `(let ((*label-nc-fn*
           ;; We need to define a labels function within the dynamic
           ;; environment as well.
           ,(eval `(labels ((,name (&rest rest)
                              (apply *label-nc-fn* rest)))
                     (lambda ,args ,@fn-body)))))
     (flet ((,name (&rest rest)
              (apply *label-nc-fn* rest)))
       ,@body)))

(defmacro using (args &body body)
  "Execute BODY in a context where only the variables within the argument list
and special bindings are in effect."
  (let ((fn (gensym)))
    `(flet-nc (,fn ,args ,@body)
       (,fn ,@args))))
