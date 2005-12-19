(defpackage :elisp-lookup (:use :cl)
            (:export :populate-table :symbol-lookup))
(in-package :elisp-lookup)

(defparameter *elisp-root* "http://www.gnu.org/software/emacs/elisp-manual/html_node/")

(defparameter *elisp-file*
  (merge-pathnames "elisp-symbols.lisp-expr"
		   #.*compile-file-pathname*))

(defvar *table* nil)

(defvar *populated-p* nil)

(defun populate-table ()
  (unless *populated-p*
    (with-open-file (r *elisp-file* :direction :input)
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
        (concatenate 'string *elisp-root*
                     val))))
