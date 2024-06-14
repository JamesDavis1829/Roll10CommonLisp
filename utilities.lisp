(defun format-name (name)
  (string-capitalize (substitute #\space #\- (string name))))

(defun make-instance-from-name (struct-name)
  (eval (read-from-string (format nil "(MAKE-~A)" struct-name))))
