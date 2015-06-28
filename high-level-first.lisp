(defpackage :pointless-macros.high-level-first
  (:use :common-lisp))

(defun wrap-quote (sxpr)
  "Given an s expression check each element (recursively) for string and wrap in \" when found"
  (mapcar
   (lambda (x)
     (cond ((typep x 'cons) (wrap-quote x))
           ((typep x 'string) (format nil "\"~a\"" x))
           (t x)))
   sxpr))

(defmacro high-level-first (&rest body)
  `(load
    (make-string-input-stream
     ,(apply #'concatenate 'string
             (mapcar (lambda (x)
                       (format nil "~a" (wrap-quote x)))
                     (reverse body))))))

(high-level-first
 (high-fn "asdf")

 (defun high-fn (x)
   (lowest-fn (format nil "HAND OFF! ~a" x)))

 (defun lowest-fn (x)
   (format t "~a~%" x)))
