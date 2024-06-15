(defparameter *base-dice* 10)
(defparameter *base-stat* 8)

(defun roll-die (number sides)
  (let ((rolls (loop for x below number collect (+ (random sides) 1)))
        (readable-die (format nil "~Ad~A" number sides)))
    (list (apply #'+ rolls) (format nil "~{~A~^ + ~}" rolls) readable-die)))

(defun const (number)
  (list number (write-to-string number) (write-to-string number)))
