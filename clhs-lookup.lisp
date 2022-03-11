(defpackage :clhs-lookup (:use :common-lisp) (:export :symbol-lookup
                                                      :populate-table))
(in-package :clhs-lookup)

;;; AMOP.
(defparameter *mop-map-file*
  (merge-pathnames "Mop_Sym.txt"
                   (or #.*compile-file-truename* *default-pathname-defaults*)))

(defparameter *mop-root* "http://www.alu.org/mop/")

(defvar *symbol-table* (make-hash-table :test 'equalp))

(defvar *populated-p* nil)

(defun populate-table ()
  (unless *populated-p*
    ;; Hyperspec
    (do-external-symbols (s :common-lisp)
      (setf (gethash (string-downcase s) *symbol-table*) (hyperspec:lookup s)))
    ;; MOP
    (with-open-file (s *mop-map-file*)
      (do ((symbol-name (read-line s nil s) (read-line s nil s))
           (url (read-line s nil s) (read-line s nil s)))
          ((eq url s) 'done)
        (setf (gethash (concatenate 'string "MOP:" symbol-name) *symbol-table*) (concatenate 'string *mop-root* url))))
    (setf *populated-p* t)))

(defun symbol-lookup (term)
  (unless *populated-p*
    (populate-table))
  (gethash term *symbol-table*))
