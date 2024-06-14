(defparameter *base-dice* 10)
(defun roll-die (number sides)
  (let ((rolls (loop for x below number collect (+ (random sides) 1))))
    (values (apply #'+ rolls) (format nil "~{~A~^ + ~}" rolls))))
