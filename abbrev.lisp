(defpackage :abbrev (:use :cl :split-sequence)
            (:export :abbrev))
(in-package :abbrev)

(defun could-be-wrap (term char-set)
  (loop for char in char-set
        if (and (> (length term) 1)
                (char= (elt term 0) char)
                (char= (elt term (1- (length term))) char))
        return char))

(defun abbrev (term &key wrap)
  (if (> (length term) 0)
      (if (char= (elt term 0) #\:)
          (abbrev (subseq term 1))
          (let ((char (could-be-wrap term '(#\* #\+))))
            (if char
                (abbrev (subseq term 1 (1- (length term))) :wrap char)
                (let ((split (split-sequence #\- term)))
                  (if (and (> (length split) 1)
                           (every #'(lambda (e) (> (length e) 0)) split))
                      (let ((abbrev (format nil "~{~C~^-~}"
                                            (mapcar #'(lambda (e)
                                                        (elt e 0)) split))))
                        (when wrap
                          (setf abbrev (format nil "~C~A~C"
                                               wrap abbrev wrap))
                          (setf term (format nil "~C~A~C"
                                             wrap term wrap)))
                        abbrev))))))))
