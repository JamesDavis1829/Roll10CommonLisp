(defun format-name (name)
  (string-capitalize (substitute #\space #\- (string name))))

(defun make-instance-from-name (struct-name)
  (eval (read-from-string (format nil "(MAKE-~A)" struct-name))))

(defmacro define-rollable (name rollable-lst compare-lookup &body body)
  (let ((function-name (read-from-string (format nil "make-~a" name))))
    `(progn
      (defun ,function-name ()
        ,@body)
      (pushnew (,function-name) ,rollable-lst :test (lambda (x y) (equal (,compare-lookup x) (,compare-lookup y)))))))
