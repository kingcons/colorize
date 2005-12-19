(defpackage :r5rs-lookup (:use :common-lisp)
            (:export :populate-table :symbol-lookup))
(in-package :r5rs-lookup)

(defparameter *r5rs-root* "http://www.schemers.org/Documents/Standards/R5RS/HTML/")

(defparameter *r5rs-file*
  (merge-pathnames "r5rs-symbols.lisp-expr"
		   #.*compile-file-pathname*))

(defvar *table* nil)

(defvar *populated-p* nil)

(defun populate-table ()
  (unless *populated-p*
    (with-open-file (r *r5rs-file* :direction :input)
      (setf *table* (make-hash-table :test #'equalp))
      (let ((s (read r)))
        (loop for i in s do (setf (gethash (car i) *table*) (cdr i))))
      'done)
    (setf *populated-p* t)))

(defun symbol-lookup (symbol)
  (unless *populated-p*
    (populate-table))
  (multiple-value-bind (val found)
      (gethash symbol *table*)
    (if found
        (concatenate 'string *r5rs-root*
                     val))))
