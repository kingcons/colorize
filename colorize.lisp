;;;; colorize.lisp

(in-package :colorize)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *coloring-types* nil)
  (defparameter *version-token* (gensym)))

(defclass coloring-type ()
  ((default-mode :initarg :default-mode :accessor coloring-type-default-mode)
   (transition-functions :initarg :transition-functions :accessor coloring-type-transition-functions)
   (fancy-name :initarg :fancy-name :accessor coloring-type-fancy-name)
   (term-formatter :initarg :term-formatter :accessor coloring-type-term-formatter)
   (formatter-initial-values :initarg :formatter-initial-values :accessor coloring-type-formatter-initial-values :initform nil)
   (formatter-after-hook :initarg :formatter-after-hook :accessor coloring-type-formatter-after-hook :initform (constantly ""))
   (autodetect-function :initarg :autodetect-function :accessor coloring-type-autodetect-function
                        :initform (constantly nil))
   (parent-type :initarg :parent-type :accessor coloring-type-parent-type
                :initform nil)
   (visible :initarg :visible :accessor coloring-type-visible
            :initform t)))

(defun find-coloring-type (type)
  (if (typep type 'coloring-type)
      type
      (cdr (assoc (symbol-name type) *coloring-types* :test #'string-equal :key #'symbol-name))))

(defun autodetect-coloring-type (name)
  (car
   (find name *coloring-types*
         :key #'cdr
         :test #'(lambda (name type)
                   (and (coloring-type-visible type)
                        (funcall (coloring-type-autodetect-function type) name))))))

(defun coloring-types ()
  (loop for type-pair in *coloring-types*
        if (coloring-type-visible (cdr type-pair))
        collect (cons (car type-pair)
                      (coloring-type-fancy-name (cdr type-pair)))))

(defun (setf find-coloring-type) (new-value type)
  (if new-value
      (let ((found (assoc type *coloring-types*)))
        (if found
            (setf (cdr found) new-value)
            (setf *coloring-types*
                  (nconc *coloring-types*
                         (list (cons type new-value))))))
      (setf *coloring-types* (remove type *coloring-types* :key #'car))))

(defvar *scan-calls* 0)

(defvar *reset-position* nil)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(mapcar #'(lambda (name)
                     (list name `(make-symbol ,(symbol-name name)))) names)
    ,@body))

(defmacro with-scanning-functions (string-param position-place mode-place mode-wait-place &body body)
  (with-gensyms (num items position not-preceded-by string item new-mode until advancing)
    `(labels ((advance (,num)
               (setf ,position-place (+ ,position-place ,num))
               t)
              (peek-any (,items &key ,not-preceded-by)
               (incf *scan-calls*)
               (let* ((,items (if (stringp ,items)
                                  (coerce ,items 'list) ,items))
                      (,not-preceded-by (if (characterp ,not-preceded-by)
                                            (string ,not-preceded-by) ,not-preceded-by))
                      (,position ,position-place)
                      (,string ,string-param))
                 (let ((,item (and
                               (< ,position (length ,string))
                               (find ,string ,items
                                     :test #'(lambda (,string ,item)
                                               #+nil
                                               (format t "looking for ~S in ~S starting at ~S~%"
                                                       ,item ,string ,position)
                                               (if (characterp ,item)
                                                   (char= (elt ,string ,position)
                                                          ,item)
                                                   (search ,item ,string :start2 ,position
                                                           :end2 (min (length ,string)
                                                                      (+ ,position (length ,item))))))))))
                   (if (characterp ,item)
                       (setf ,item (string ,item)))
                   (if
                    (if ,item
                        (if ,not-preceded-by
                            (if (>= (- ,position (length ,not-preceded-by)) 0)
                                (not (string= (subseq ,string
                                                      (- ,position (length ,not-preceded-by))
                                                      ,position)
                                              ,not-preceded-by))
                                t)
                            t)
                        nil)
		    ,item
                    (progn
                      (and *reset-position*
                           (setf ,position-place *reset-position*))
                      nil)))))
	      (scan-any (,items &key ,not-preceded-by)
		(let ((,item (peek-any ,items :not-preceded-by ,not-preceded-by)))
		  (and ,item (advance (length ,item)))))
	      (peek (,item &key ,not-preceded-by)
		(peek-any (list ,item) :not-preceded-by ,not-preceded-by))
              (scan (,item &key ,not-preceded-by)
               (scan-any (list ,item) :not-preceded-by ,not-preceded-by)))
      (macrolet ((set-mode (,new-mode &key ,until (,advancing t))
                   (list 'progn
                         (list 'setf ',mode-place ,new-mode)
                         (list 'setf ',mode-wait-place
                               (list 'lambda (list ',position)
                                     (list 'let (list (list '*reset-position* ',position))
                                           (list 'values ,until ,advancing)))))))
        ,@body))))

(defvar *formatter-local-variables*)

(defmacro define-coloring-type (name fancy-name &key default-mode transitions formatters
                                autodetect parent formatter-variables (formatter-after-hook '(constantly ""))
                                invisible)
  (with-gensyms (parent-type term type string current-mode position position-foobage mode-wait new-position advance)
    `(let ((,parent-type (or (find-coloring-type ,parent)
                             (and ,parent
                                  (error "No such coloring type: ~S" ,parent)))))
      (setf (find-coloring-type ,name)
       (make-instance 'coloring-type
        :fancy-name ',fancy-name
        :default-mode (or ',default-mode
                          (if ,parent-type (coloring-type-default-mode ,parent-type)))
        ,@(if autodetect
              `(:autodetect-function ,autodetect))
        :parent-type ,parent-type
        :visible (not ,invisible)
        :formatter-initial-values (lambda nil
                                    (list* ,@(mapcar #'(lambda (e)
                                                         `(cons ',(car e) ,(second e)))
                                                     formatter-variables)
                                           (if ,parent-type
                                               (funcall (coloring-type-formatter-initial-values ,parent-type))
                                               nil)))
        :formatter-after-hook (lambda nil
                                (symbol-macrolet ,(mapcar #'(lambda (e)
                                                              `(,(car e) (cdr (assoc ',(car e) *formatter-local-variables*))))
                                                          formatter-variables)
                                    (concatenate 'string
                                                 (funcall ,formatter-after-hook)
                                                 (if ,parent-type
                                                     (funcall (coloring-type-formatter-after-hook ,parent-type))
                                                     ""))))
        :term-formatter
        (symbol-macrolet ,(mapcar #'(lambda (e)
                                      `(,(car e) (cdr (assoc ',(car e) *formatter-local-variables*))))
                                  formatter-variables)
            (lambda (,term)
              (labels ((call-parent-formatter (&optional (,type (car ,term))
                                                         (,string (cdr ,term)))
                         (if ,parent-type
                             (funcall (coloring-type-term-formatter ,parent-type)
                                      (cons ,type ,string))))
                       (call-formatter (&optional (,type (car ,term))
                                                  (,string (cdr ,term)))
                         (funcall
                          (case (first ,type)
                            ,@formatters
                            (t (lambda (,type text)
                                 (call-parent-formatter ,type text))))
                          ,type ,string)))
                (call-formatter))))
        :transition-functions
        (list
         ,@(loop for transition in transitions
                 collect (destructuring-bind (mode &rest table) transition
                           `(cons ',mode
                             (lambda (,current-mode ,string ,position)
                               (let ((,mode-wait (constantly nil))
                                     (,position-foobage ,position))
                                 (with-scanning-functions ,string ,position-foobage
                                                          ,current-mode ,mode-wait
                                                          (let ((*reset-position* ,position))
                                                            (cond ,@table))
                                                          (values ,position-foobage ,current-mode
                                                                  (lambda (,new-position)
                                                                    (setf ,position-foobage ,new-position)
                                                                    (let ((,advance (nth-value 1 (funcall ,mode-wait ,position-foobage))))
                                                                      (values ,position-foobage ,advance)))))
                                 )))))))))))

(defun full-transition-table (coloring-type-object)
  (let ((parent (coloring-type-parent-type coloring-type-object)))
    (if parent
        (append (coloring-type-transition-functions coloring-type-object)
                (full-transition-table parent))
        (coloring-type-transition-functions coloring-type-object))))

(defun scan-string (coloring-type string)
  (let* ((coloring-type-object (or (find-coloring-type coloring-type)
                                   (error "No such coloring type: ~S" coloring-type)))
         (transitions (full-transition-table coloring-type-object))
         (result nil)
         (low-bound 0)
         (current-mode (coloring-type-default-mode coloring-type-object))
         (mode-stack nil)
         (current-wait (constantly nil))
         (wait-stack nil)
         (current-position 0)
         (*scan-calls* 0))
    (flet ((finish-current (new-position new-mode new-wait &key (extend t) push pop)
             (let ((to (if extend new-position current-position)))
               (if (> to low-bound)
                   (setf result (nconc result
                                       (list (cons (cons current-mode mode-stack)
                                                   (subseq string low-bound
                                                           to))))))
               (setf low-bound to)
               (when pop
                 (pop mode-stack)
                 (pop wait-stack))
               (when push
                 (push current-mode mode-stack)
                 (push current-wait wait-stack))
               (setf current-mode new-mode
                     current-position new-position
                     current-wait new-wait))))
      (loop
       (if (> current-position (length string))
           (return-from scan-string
             (progn
               (format *trace-output* "Scan was called ~S times.~%"
                       *scan-calls*)
               (finish-current (length string) nil (constantly nil))
               result))
           (or
            (loop for transition in
                  (mapcar #'cdr
                          (remove current-mode transitions
                                  :key #'car
                                  :test-not #'(lambda (a b)
                                                (or (eql a b)
                                                    (if (listp b)
                                                        (member a b))))))
                  if
                  (and transition
                       (multiple-value-bind
                             (new-position new-mode new-wait)
                           (funcall transition current-mode string current-position)
                         (when (> new-position current-position)
                           (finish-current new-position new-mode new-wait :extend nil :push t)
                           t)))
                  return t)
            (multiple-value-bind
                  (pos advance)
                (funcall current-wait current-position)
              #+nil
              (format t "current-wait returns ~S ~S (mode is ~S, pos is ~S)~%" pos advance current-mode current-position)
              (and pos
                   (when (> pos current-position)
                     (finish-current (if advance
                                         pos
                                         current-position)
                                     (car mode-stack)
                                     (car wait-stack)
                                     :extend advance
                                     :pop t)
                     t)))
            (progn
              (incf current-position)))
           )))))

(defun format-scan (coloring-type scan)
  (let* ((coloring-type-object (or (find-coloring-type coloring-type)
                                   (error "No such coloring type: ~S" coloring-type)))
         (color-formatter (coloring-type-term-formatter coloring-type-object))
         (*formatter-local-variables* (funcall (coloring-type-formatter-initial-values coloring-type-object))))
    (format nil "<span class=\"~A\">~{~A~}~A</span>"
	    *css-background-class*
            (mapcar color-formatter scan)
            (funcall (coloring-type-formatter-after-hook coloring-type-object)))))

(defun html-colorization (coloring-type string)
  (format-scan coloring-type
               (mapcar #'(lambda (p)
                           (cons (car p)
                                 (let ((tt
                                        (html-encode:encode-for-pre (cdr p))))
                                   (if (and (> (length tt) 0)
                                            (char= (elt tt (1- (length tt))) #\>))
                                       (format nil "~A~%" tt) tt))))
                       (scan-string coloring-type
                                    string))))

(defun colorize-file-to-stream (coloring-type input-file-name s2 &key (wrap t) (css-background "default"))
  (let* ((input-file (if (pathname-type (merge-pathnames input-file-name))
                         (merge-pathnames input-file-name)
                         (make-pathname :type "lisp"
                                        :defaults (merge-pathnames input-file-name))))
         (*css-background-class* css-background))
    (with-open-file (s input-file :direction :input)
      (let ((lines nil)
            (string nil))
        (block done
          (loop (let ((line (read-line s nil nil)))
                  (if line
                      (push line lines)
                      (return-from done)))))
        (setf string (format nil "~{~A~%~}"
                             (nreverse lines)))
        (if wrap
            (format s2
                    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">
<html><head><style type=\"text/css\">~A~%~A</style><body>
<table width=\"100%\"><tr><td class=\"~A\">
<tt>~A</tt>
</tr></td></table></body></html>"
                    *coloring-css*
                    (make-background-css "white")
                    *css-background-class*
                    (html-colorization coloring-type string))
            (write-string (html-colorization coloring-type string) s2))))))

(defun colorize-file (coloring-type input-file-name &optional output-file-name)
  (let* ((input-file (if (pathname-type (merge-pathnames input-file-name))
                         (merge-pathnames input-file-name)
                         (make-pathname :type "lisp"
                                        :defaults (merge-pathnames input-file-name))))
         (output-file (or output-file-name
                          (make-pathname :type "html"
                                         :defaults input-file))))
    (with-open-file (s2 output-file :direction :output :if-exists :supersede)
      (colorize-file-to-stream coloring-type input-file-name s2))))
