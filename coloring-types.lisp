;; coloring-types.lisp

(in-package :colorize)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *version-token* (gensym)))

(defparameter *symbol-characters*
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ*!%$&+-1234567890")

(defparameter *non-constituent*
  '(#\space #\tab #\newline #\linefeed #\page #\return
    #\" #\' #\( #\) #\, #\; #\` #\[ #\]))

(defparameter *special-forms*
  '("let" "load-time-value" "quote" "macrolet" "progn" "progv" "go" "flet" "the"
    "if" "throw" "eval-when" "multiple-value-prog1" "unwind-protect" "let*"
    "labels" "function" "symbol-macrolet" "block" "tagbody" "catch" "locally"
    "return-from" "setq" "multiple-value-call"))

(defparameter *common-macros*
  '("loop" "cond" "lambda"))

(defparameter *open-parens* '(#\())
(defparameter *close-parens* '(#\)))

(define-coloring-type :lisp "Basic Lisp"
  :default-mode :first-char-on-line
  :transitions
  (((:in-list)
    ((or
      (scan-any *symbol-characters*)
      (and (scan #\.) (scan-any *symbol-characters*))
      (and (scan #\\) (advance 1)))
     (set-mode :symbol
               :until (scan-any *non-constituent*)
               :advancing nil))
    ((or (scan #\:) (scan "#:"))
     (set-mode :keyword
               :until (scan-any *non-constituent*)
               :advancing nil))
    ((scan "#\\")
     (let ((count 0))
       (set-mode :character
                 :until (progn
                          (incf count)
                          (if (> count 1)
                              (scan-any *non-constituent*)))
                 :advancing nil)))
    ((scan #\")
     (set-mode :string
               :until (scan #\")))
    ((scan #\;)
     (set-mode :comment
               :until (scan #\newline)))
    ((scan "#|")
     (set-mode :multiline
               :until (scan "|#")))
    ((scan #\()
     (set-mode :in-list
               :until (scan #\)))))
   ((:normal :first-char-on-line)
    ((scan #\()
     (set-mode :in-list
               :until (scan #\))))
    ((scan #\;)
     (set-mode :comment
               :until (scan #\newline)))
    ((scan "#|")
     (set-mode :multiline
               :until (scan "|#")))
    ((advance 1)
     (set-mode :normal
               :until (scan #\newline))))
   (:multiline
    ((scan "#|")
     (set-mode :multiline
               :until (scan "|#"))))
   ((:symbol :keyword :escaped-symbol :string)
    ((scan #\\)
     (let ((count 0))
       (set-mode :single-escaped
                 :until (progn
                          (incf count)
                          (if (< count 2)
                              (advance 1))))))))
  :formatter-variables ((paren-counter 0))
  :formatter-after-hook (lambda nil
			  (close-dangling-parenthesis paren-counter))
  :formatters
  (((:normal :first-char-on-line)
    (lambda (type s)
      (declare (ignore type))
      s))
   ((:in-list)
    (lambda (type s)
      (declare (ignore type))
      (labels ((color-parens (s)
                 (let ((paren-pos (find-if-not #'null
                                               (mapcar #'(lambda (c)
                                                           (position c s))
                                                       (append *open-parens*
                                                               *close-parens*)))))
                   (if paren-pos
                       (let ((before-paren (subseq s 0 paren-pos))
                             (after-paren (subseq s (1+ paren-pos)))
                             (paren (elt s paren-pos)))

			 (format-parenthesis paren 
					     :open-parens *open-parens*
					     :close-parens *close-parens*
					     :paren-counter paren-counter
					     :before-paren before-paren
					     :after-paren (color-parens after-paren)))
                       s))))
        (color-parens s))))
   ((:symbol :escaped-symbol)
    (lambda (type s)
      (declare (ignore type))
      (let* ((colon (position #\: s :from-end t))
             (new-s (or (and colon (subseq s (1+ colon))) s)))
        (cond
          ((or
            (member new-s *common-macros* :test #'string-equal)
            (member new-s *special-forms* :test #'string-equal)
            (some #'(lambda (e)
                      (and (> (length new-s) (length e))
                           (string-equal e (subseq new-s 0 (length e)))))
                  '("WITH-" "DEF")))
           (format nil "<i><span class=\"symbol\">~A</span></i>" s))
          ((and (> (length new-s) 2)
                (char= (elt new-s 0) #\*)
                (char= (elt new-s (1- (length new-s))) #\*))
           (format nil "<span class=\"special\">~A</span>" s))
          (t s)))))
   (:keyword (lambda (type s)
      (declare (ignore type))
               (format nil "<span class=\"keyword\">~A</span>"
                       s)))
   ((:comment :multiline)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"comment\">~A</span>"
              s)))
   ((:character)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"character\">~A</span>"
              s)))
   ((:string)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"string\">~A</span>"
              s)))
   ((:single-escaped)
    (lambda (type s)
      (call-formatter (cdr type) s)))
   ((:syntax-error)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"syntaxerror\">~A</span>"
              s)))))

(define-coloring-type :scheme "Scheme"
  :parent :lisp
  :transitions
  (((:normal :in-list)
    ((scan "...")
     (set-mode :symbol
               :until (scan-any *non-constituent*)
               :advancing nil))
    ((scan #\[)
     (set-mode :in-list
               :until (scan #\])))))
  :formatters
  (((:in-list)
    (lambda (type s)
      (declare (ignore type s))
      (let ((*open-parens* (cons #\[ *open-parens*))
            (*close-parens* (cons #\] *close-parens*)))
        (call-parent-formatter))))
   ((:symbol :escaped-symbol)
    (lambda (type s)
      (declare (ignore type))
      (let ((result (if (find-package :r5rs-lookup)
                         (funcall (symbol-function (intern "SYMBOL-LOOKUP" :r5rs-lookup))
                                  s))))
        (if result
            (format nil "<a href=\"~A\" class=\"symbol\">~A</a>"
                    result (call-parent-formatter))
            (call-parent-formatter)))))))

(define-coloring-type :clojure "Clojure"
  :parent :lisp
  :transitions
  (((:normal :in-list)
    ((scan "...")
     (set-mode :symbol
               :until (scan-any *non-constituent*)
               :advancing nil))
    ((scan #\[)
     (set-mode :in-list
               :until (scan #\])))
    ((scan #\{)
     (set-mode :in-list
               :until (scan #\})))))
  :formatters
  (((:in-list)
    (lambda (type s)
      (declare (ignore type s))
      (let ((*open-parens* (append '(#\{ #\[) *open-parens*))
            (*close-parens* (append '(#\} #\]) *close-parens*)))
        (call-parent-formatter))))
   ((:symbol :escaped-symbol)
    (lambda (type s)
      (declare (ignore type s))
      (call-parent-formatter)))))

(define-coloring-type :elisp "Emacs Lisp"
  :parent :lisp
  :formatters
  (((:symbol :escaped-symbol)
    (lambda (type s)
      (declare (ignore type))
      (let ((result (if (find-package :elisp-lookup)
                         (funcall (symbol-function (intern "SYMBOL-LOOKUP" :elisp-lookup))
                                  s))))
        (if result
            (format nil "<a href=\"~A\" class=\"symbol\">~A</a>"
                    result (call-parent-formatter))
            (call-parent-formatter)))))))

(define-coloring-type :common-lisp "Common Lisp"
  :parent :lisp
  :transitions
  (((:normal :in-list)
    ((scan #\|)
     (set-mode :escaped-symbol
               :until (scan #\|)))))
  :formatters 
  (((:symbol :escaped-symbol)
    (lambda (type s)
      (declare (ignore type))
      (let* ((colon (position #\: s :from-end t :test #'char=))
             (to-lookup (if colon (subseq s (1+ colon)) s))
             (result (if (find-package :clhs-lookup)
                         (funcall (symbol-function (intern "SYMBOL-LOOKUP" :clhs-lookup))
                                  to-lookup))))
        (if result
            (format nil "<a href=\"~A\" class=\"symbol\">~A</a>"
                    result (call-parent-formatter))
            (call-parent-formatter)))))))

(define-coloring-type :common-lisp-file "Common Lisp File"
  :parent :common-lisp
  :default-mode :in-list
  :invisible t)

(defvar *c-open-parens* "([{")
(defvar *c-close-parens* ")]}")

(defvar *c-reserved-words*
  '("auto"   "break"  "case"   "char"   "const"
    "continue" "default" "do"     "double" "else"
    "enum"   "extern" "float"  "for"    "goto"
    "if"     "int"    "long"   "register" "return"
    "short"  "signed" "sizeof" "static" "struct"
    "switch" "typedef" "union"  "unsigned" "void"
    "volatile" "while"  "__restrict" "_Bool"))

(defparameter *c-begin-word* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")
(defparameter *c-terminators* '(#\space #\return #\tab #\newline #\. #\/ #\- #\* #\+ #\{ #\} #\( #\) #\' #\" #\[ #\] #\< #\> #\#))

(define-coloring-type :basic-c "Basic C"
  :default-mode :normal
  :invisible t
  :transitions
  ((:normal
    ((scan-any *c-begin-word*)
     (set-mode :word-ish
               :until (scan-any *c-terminators*)
               :advancing nil))
    ((scan "/*")
     (set-mode :comment
               :until (scan "*/")))
    
    ((or
      (scan-any *c-open-parens*)
      (scan-any *c-close-parens*))
     (set-mode :paren-ish
               :until (advance 1)
               :advancing nil))
    ((scan #\")
     (set-mode :string
               :until (scan #\")))
    ((or (scan "'\\")
         (scan #\'))
     (set-mode :character
               :until (advance 2))))
   (:string
    ((scan #\\)
     (set-mode :single-escape
               :until (advance 1)))))
  :formatter-variables
  ((paren-counter 0))
  :formatter-after-hook (lambda nil
			  (close-dangling-parenthesis paren-counter))
  :formatters
  ((:normal
    (lambda (type s)
      (declare (ignore type))
      s))
   (:comment
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"comment\">~A</span>"
              s)))
   (:string
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"string\">~A</span>"
              s)))
   (:character
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"character\">~A</span>"
              s)))
   (:single-escape
    (lambda (type s)
      (call-formatter (cdr type) s)))
   (:paren-ish
    (lambda (type s)
      (declare (ignore type))
      (if (eql (length s) 1)
	  (format-parenthesis (elt s 0) 
			      :open-parens *c-open-parens*
			      :close-parens *c-close-parens*
			      :paren-counter paren-counter)
	  s)))
   (:word-ish
    (lambda (type s)
      (declare (ignore type))
      (if (member s *c-reserved-words* :test #'string=)
          (format nil "<span class=\"symbol\">~A</span>" s)
          s)))
   ))

(define-coloring-type :c "C"
  :parent :basic-c
  :transitions
  ((:normal
    ((scan #\#)
     (set-mode :preprocessor
               :until (scan-any '(#\return #\newline)))))
   (:normal
    ((scan "//")
     (set-mode :comment
               :until (scan-any '(#\return #\newline))))))
  :formatters
  ((:preprocessor
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"special\">~A</span>" s)))))

(defvar *c++-reserved-words*
  '("asm"          "auto"      "bool"     "break"            "case"
    "catch"        "char"      "class"   "const"            "const_cast"
    "continue"     "default"   "delete"   "do"               "double"
    "dynamic_cast" "else"      "enum"     "explicit"         "export"
    "extern"       "false"     "float"    "for"              "friend"
    "goto"         "if"        "inline"   "int"              "long"
    "mutable"      "namespace" "new"      "operator"         "private"
    "protected"    "public"    "register" "reinterpret_cast" "return"
    "short"        "signed"    "sizeof"   "static"           "static_cast"
    "struct"       "switch"    "template" "this"             "throw"
    "true"         "try"       "typedef"  "typeid"           "typename"
    "union"        "unsigned"  "using"    "virtual"          "void"
    "volatile"     "wchar_t"   "while"))

(define-coloring-type :c++ "C++"
  :parent :c
  :transitions
  ()
  :formatters
  ((:word-ish
    (lambda (type s)
      (declare (ignore type))
      (if (member s *c++-reserved-words* :test #'string=)
          (format nil "<span class=\"symbol\">~A</span>"
                  s)
          s)))))

(defvar *java-reserved-words*
  '("abstract"     "boolean"      "break"    "byte"         "case" 
    "catch"        "char"         "class"   "const"        "continue" 
    "default"      "do"           "double"   "else"         "extends" 
    "final"        "finally"      "float"    "for"          "goto" 
    "if"           "implements"   "import"   "instanceof"   "int" 
    "interface"    "long"         "native"   "new"          "package" 
    "private"      "protected"    "public"   "return"       "short" 
    "static"       "strictfp"     "super"    "switch"       "synchronized" 
    "this"         "throw"        "throws"   "transient"    "try" 
    "void"         "volatile"     "while"))

(define-coloring-type :java "Java"
  :parent :c++
  :formatters
  ((:word-ish
    (lambda (type s)
      (declare (ignore type))
      (if (member s *java-reserved-words* :test #'string=)
          (format nil "<span class=\"symbol\">~A</span>"
                  s)
          s)))))

(let ((terminate-next nil))
  (define-coloring-type :objective-c "Objective C"
    :transitions
    ((:normal
      ((scan #\[)
       (set-mode :begin-message-send
		 :until (advance 1)
		 :advancing nil))
      ((scan #\])
       (set-mode :end-message-send
		 :until (advance 1)
		 :advancing nil))
      ((scan-any *c-begin-word*)
       (set-mode :word-ish
		 :until (or
			 (and (peek-any '(#\:))
			      (setf terminate-next t))
			 (and terminate-next (progn
					       (setf terminate-next nil)
					       (advance 1)))
			 (scan-any *c-terminators*))
		 :advancing nil)))
     (:word-ish
      #+nil
      ((scan #\:)
       (format t "hi~%")
       (set-mode :word-ish :until (advance 1) :advancing nil)
       (setf terminate-next t))))
  :parent :c++
  :formatter-variables ((is-keyword nil) (in-message-send nil))
  :formatters
  ((:begin-message-send
    (lambda (type s)
      (setf is-keyword nil)
      (setf in-message-send t)
      (call-formatter (cons :paren-ish type) s)))
   (:end-message-send
    (lambda (type s)
      (setf is-keyword nil)
      (setf in-message-send nil)
      (call-formatter (cons :paren-ish type) s)))
   (:word-ish
    (lambda (type s)
      (declare (ignore type))
      (prog1
	  (let ((result (if (find-package :cocoa-lookup)
			    (funcall (symbol-function (intern "SYMBOL-LOOKUP" :cocoa-lookup))
				     s))))
	    (if result
		(format nil "<a href=\"~A\" class=\"symbol\">~A</a>"
			result s)
		(if (member s *c-reserved-words* :test #'string=)
		    (format nil "<span class=\"symbol\">~A</span>" s)
		    (if in-message-send
			(if is-keyword
			    (format nil "<span class=\"keyword\">~A</span>" s)
			    s)
			s))))
	(setf is-keyword (not is-keyword))))))))


(defvar *erlang-open-parens* "([{")
(defvar *erlang-close-parens* ")]}")

(defvar *erlang-reserved-words*
  '("after" "andalso" "begin" "catch" "case" "end" "fun" "if" "of" "orelse"
    "receive" "try" "when" "query" "is_atom" "is_binary" "is_constant"
    "is_float" "is_function" "is_integer" "is_list" "is_number" "is_pid"
    "is_port" "is_reference" "is_tuple" "is_record" "abs" "element" "float"
    "hd" "tl" "length" "node" "round" "self" "size" "trunc" "alive" "apply"
    "atom_to_list" "binary_to_list" "binary_to_term" "concat_binary"
    "date" "disconnect_node" "erase" "exit" "float_to_list" "garbage_collect"
    "get" "get_keys" "group_leader" "halt" "integer_to_list" "internal_bif"
    "link" "list_to_atom" "list_to_binary" "list_to_float" "list_to_integer"
    "make_ref" "node_link" "node_unlink" "notalive" "open_port" "pid_to_list"
    "process_flag" "process_info" "processes" "put" "register" "registered"
    "setelement" "spawn" "spawn_link" "split_binary" "statistics"
    "term_to_binary" "time" "throw" "trace" "trunc" "tuple_to_list"
    "unlink" "unregister" "whereis"))

(defparameter *erlang-begin-word* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")
(defparameter *erlang-begin-fun* "abcdefghijklmnopqrstuvwxyz")
(defparameter *erlang-begin-var* "ABCDEFGHIJKLMNOPQRSTUVWXYZ_")
(defparameter *erlang-terminators* '(#\space #\return #\tab #\newline #\. #\; #\, #\/ #\- #\* #\+ #\( #\) #\' #\" #\[ #\] #\< #\> #\{ #\}))

(define-coloring-type :erlang "Erlang"
  :default-mode :first-char-on-line
  :transitions
  (((:normal :paren-ish)
    ((scan "%")
     (set-mode :comment
               :until (scan #\newline)))
    ((scan-any *erlang-begin-var*)
     (set-mode :variable
               :until (scan-any *erlang-terminators*)
               :advancing nil))
    ((scan-any *erlang-begin-word*)
     (set-mode :word-ish
               :until (scan-any *erlang-terminators*)
               :advancing nil))
    ((or
      (scan-any *erlang-open-parens*)
      (scan-any *erlang-close-parens*))
     (set-mode :paren-ish
               :until (advance 1)
               :advancing nil))
    ((scan #\")
     (set-mode :string
               :until (scan #\")))
    ((scan #\')
     (set-mode :atom
               :until (scan #\')))
    ((scan #\?)
     (set-mode :macro
               :until (scan-any *erlang-terminators*)))
    ((scan #\$)
     (set-mode :char
               :until (scan-any *erlang-terminators*)))
    ((scan #\newline)
     (set-mode :first-char-on-line)))
   
   ((:function :attribute)
    ((or
      (scan-any *erlang-open-parens*)
      (scan-any *erlang-close-parens*))
     (set-mode :paren-ish
               :until (advance 1)
               :advancing nil))
    ((scan-any *erlang-terminators*)
     (set-mode :normal
               :until (scan #\newline))))
   
   (:first-char-on-line
    ((scan "%")
     (set-mode :comment
               :until (scan #\newline)))
    ((scan-any *erlang-begin-fun*)
     (set-mode :function
               :until (scan #\newline)
               :advancing nil))
    ((scan "-")
     (set-mode :attribute
               :until (scan #\newline)
               :advancing nil))
    ((advance 1)
     (set-mode :normal
               :until (scan #\newline))))
   (:string
    ((scan #\\)
     (set-mode :single-escape
               :until (advance 1)))))
  :formatter-variables
  ((paren-counter 0))
  :formatter-after-hook (lambda nil
			  (close-dangling-parenthesis paren-counter))
  :formatters
  (((:normal :first-char-on-line)
    (lambda (type s)
      (declare (ignore type))
      s))
   (:comment
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"comment\">~A</span>"
              s)))
   (:string
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"string\">~A</span>"
              s)))
   (:variable
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"variable\">~A</span>"
              s)))
   (:function
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"function\">~A</span>"
              s)))
   (:attribute
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"attribute\">~A</span>"
              s)))
   (:macro
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"macro\">~A</span>"
              s)))
   (:atom
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"atom\">~A</span>"
              s)))
   (:char
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"character\">~A</span>"
              s)))
   (:single-escape
    (lambda (type s)
      (call-formatter (cdr type) s)))
   (:paren-ish
    (lambda (type s)
      (declare (ignore type))
      (if (eql (length s) 1)
	  (format-parenthesis (elt s 0)
			      :open-parens *erlang-open-parens*
			      :close-parens *erlang-open-parens*
			      :paren-counter paren-counter)
	  s)))
   (:word-ish
    (lambda (type s)
      (declare (ignore type))
      (if (member s *erlang-reserved-words* :test #'string=)
          (format nil "<span class=\"symbol\">~A</span>" s)
          s)))
   ))

(defvar *python-reserved-words*
  '("and"       "assert"        "break"         "class"         "continue"
    "def"       "del"           "elif"          "else"          "except"
    "exec"      "finally"       "for"           "from"          "global"
    "if"        "import"        "in"            "is"            "lambda"
    "not"       "or"            "pass"          "print"         "raise"
    "return"    "try"           "while"         "yield"))

(define-coloring-type :python "Python"
  :default-mode :normal
  :transitions
  ((:normal
    ((or
      (scan-any *c-open-parens*)
      (scan-any *c-close-parens*))
     (set-mode :paren-ish
               :until (advance 1)
               :advancing nil)) 
    ((scan #\#)
     (set-mode :comment
	       :until (scan-any '(#\return #\newline))))
    ((scan #\")
     (set-mode :string
               :until (scan #\")))
    ((scan "\"\"\"")
     (set-mode :string
	       :until (scan "\"\"\"")))
    ((scan "'''")
     (set-mode :string
	       :until (scan "'''")))
    ((scan #\')
     (set-mode :string
	       :until (scan #\')))
    ((scan "@")
     (set-mode :decorator
	       :until (scan-any *non-constituent*)
	       :advancing nil))
    ((scan "def")
     (set-mode :def
	       :until (scan-any '(#\: #\())
	       :advancing nil))
    ((scan "class")
     (set-mode :def
	       :until (scan-any '(#\: #\())
	       :advancing nil))
    ;; Python bug-fix from sjamaan of #scheme, 2010/06/26.
    ((scan-any *c-begin-word*)
     (set-mode :word-ish
               :until (scan-any *c-terminators*)
               :advancing nil)))
   (:string
    ((scan #\\)
     (set-mode :single-escape
               :until (advance 1)))))
  :formatter-variables ((paren-counter 0))
  :formatters
  ((:normal
    (lambda (type s)
      (declare (ignore type))
      s))
   (:comment
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"comment\">~A</span>"
              s)))
   (:string
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"string\">~A</span>"
              s)))
   (:character
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"character\">~A</span>"
              s)))
   (:single-escape
    (lambda (type s)
      (call-formatter (cdr type) s)))
   (:paren-ish
    (lambda (type s)
      (declare (ignore type))
      (if (eql (length s) 1)
	  (format-parenthesis (elt s 0) 
			      :open-parens *c-open-parens*
			      :close-parens *c-close-parens*
			      :paren-counter paren-counter)
	  s)))
   (:def
       (lambda (type s)
	 (declare (ignore type))
	 (format nil "<span class=\"special\">~A</span><span
class=\"keyword\">~A</span>"
		 (subseq s 0 (position #\Space s))
		 (subseq s (position #\Space s)))))
   (:decorator
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"symbol\">~A</span>" s)))
   (:word-ish
    (lambda (type s)
      (declare (ignore type))
      (if (member s *python-reserved-words* :test #'string=)
	  (format nil "<span class=\"symbol\">~A</span>"
		  s)
	  s)))))

(defvar *haskell-open-parens* "([{")

(defvar *haskell-close-parens* ")]}")

(defvar *haskell-in-word*
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")

(defvar *haskell-begin-id* "abcdefghijklmnopqrstuvwxyz")

(defvar *haskell-begin-cons* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defvar *haskell-in-symbol* "!#$%&*+./<=>?@\\^|-~:")

(defvar *haskell-reserved-symbols*
  '(".." "::" "@" "~" "=" "-&gt;" "&lt;-" "|" "\\"))

(defvar *haskell-reserved-words*
  '("case" "class" "data" "default" "deriving" "do" "else" "if"
    "import" "in" "infix" "infixl" "infixr" "instance" "let" "module"
    "newtype" "of" "then" "type" "where"))

(defvar *haskell-non-constituent*
  '(#\space #\return #\tab #\newline #\{ #\} #\( #\) #\" #\[ #\]))

(define-coloring-type :haskell "Haskell"
  :default-mode :normal
  :transitions
  (((:normal)
    ((scan-any *haskell-in-word*)
     (set-mode :identifier
	       :until (or (scan-any *haskell-non-constituent*)
			  (scan-any *haskell-in-symbol*))
	       :advancing nil))
    ((scan "--")
     (set-mode :comment
	       :until (scan-any '(#\return #\newline))
	       :advancing nil))
    ((scan "{-")
     (set-mode :multi-comment
	       :until (scan "-}")))
    ((scan #\")
     (set-mode :string
	       :until (scan #\")))
    ((scan #\`)
     (set-mode :backquote
	       :until (scan #\`)))
    ((scan "'")
     (set-mode :char
	       :until (scan #\')))
    ((scan-any *haskell-in-symbol*)
     (set-mode :symbol
	       :until (or (scan-any *haskell-non-constituent*)
			  (scan-any *haskell-in-word*)
			  (scan #\'))
	       :advancing nil))
    ((or (scan-any *haskell-open-parens*)
	 (scan-any *haskell-close-parens*))
     (set-mode :parenlike
	       :until (advance 1)
	       :advancing nil))
    ((scan #\newline)
     (set-mode :newline
	       :until (advance 1)
	       :advancing nil)))
   ((:string)
    ((scan #\\)
     (set-mode :single-escape
               :until (advance 1))))
   ((:char)
    ((scan #\\)
     (set-mode :single-escape
               :until (advance 1)))))
  :formatter-variables
  ((paren-counter 0)
   (beginning-of-line t))
  :formatter-after-hook (lambda nil
			  (close-dangling-parenthesis paren-counter))
  :formatters
  (((:normal)
    (lambda (type s)
      (declare (ignore type))
      (cond (beginning-of-line
	     (setq beginning-of-line nil)
	     (if (and (> (length s) 0)
		      (char= (elt s 0) #\space))
		 (concatenate 'string "&nbsp;" (subseq s 1))
                 s))
	    (t s))))
   ((:newline)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line t)
      s))
   ((:backquote)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line nil)
      (if (find (elt s 1) *haskell-begin-cons*)
	  (format nil "<span class=\"variable\">~A</span>"
		  s)
          (format nil "<span class=\"atom\">~A</span>"
                  s))))
   ((:comment :multi-comment)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line nil)
      (format nil "<span class=\"comment\">~A</span>"
	      s)))
   ((:string)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line nil)
      (format nil "<span class=\"string\">~A</span>"
	      s)))
   ((:char)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line nil)
      (format nil "<span class=\"character\">~A</span>"
	      s)))
   ((:identifier)
    (lambda (type s)
      (declare (ignore type))
      (prog1
	  (cond ((find (elt s 0) *haskell-begin-cons*)
		 (format nil "<span class=\"variable\">~A</span>" s))
		((member s *haskell-reserved-words* :test #'string=)
		 (format nil "<span class=\"keyword\">~A</span>" s))
		(beginning-of-line
		 (format nil "<span class=\"function\">~A</span>" s))
		(t s))
	(setq beginning-of-line nil))))
   ((:symbol)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line nil)
      (cond ((member s *haskell-reserved-symbols* :test #'string=)
	     (format nil "<span class=\"keyword\">~A</span>" s))
	    ((char= (elt s 0) #\:)
	     (format nil "<span class=\"variable\">~A</span>" s))
	    (t (format nil "<span class=\"atom\">~A</span>" s)))))
   ((:single-escape)
    (lambda (type s)
      (call-formatter (cdr type) s)))
   ((:parenlike)
    (lambda (type s)
      (declare (ignore type))
      (setq beginning-of-line nil)
      (if (eql (length s) 1)
	  (format-parenthesis (elt s 0)
			      :open-parens *haskell-open-parens*
			      :close-parens *haskell-close-parens*
			      :paren-counter paren-counter)
	  s)))))

(define-coloring-type :diff "Unified Context Diff"
  :default-mode :first-char-on-line
  :transitions
  (((:first-char-on-line :normal :index :index-file :git-index :git-index-file :git-diff)
    ((scan #\newline)
     (set-mode :first-char-on-line)))
   ((:first-char-on-line)
    ((scan "@@")
     (set-mode :range-information
	       :until (scan "@@")))
    ((scan "===")
     (set-mode :separator
	       :until (scan #\newline)))
    ((scan "--- ")
     (set-mode :file-from
	       :until (scan #\newline)))
    ((scan "+++ ")
     (set-mode :file-to
	       :until (scan #\newline)))
    ((scan "diff --git ")
     (set-mode :git-diff
	       :until (scan #\newline)))
    ((scan "index ")
     (set-mode :git-index))
    ((scan "Index: ")
     (set-mode :index))
    ((scan #\-)
     (set-mode :diff-deleted
	       :until (scan #\newline)))
    ((scan #\+)
     (set-mode :diff-added
	       :until (scan #\newline))) 
    ((advance 1)
     (set-mode :normal)))
   ((:git-diff)
    ((scan "a/")
     (set-mode :git-index-file))
    ((scan "b/")
     (set-mode :git-index-file)))
   ((:git-index-file)
    ((scan #\space)
     (set-mode :git-diff)))
   ((:index)
    ((advance 1)
     (set-mode :index-file))))
  :formatters
  (((:normal :first-char-on-line)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"diff-normal\">~A</span>" s)))
   ((:separator :file-from :file-to)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"string\">~A</span>" s)))
   ((:range-information)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"variable\">~A</span>" s)))
   ((:diff-added)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"diff-added\">~A</span>" s)))
   ((:diff-deleted)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"diff-deleted\">~A</span>" s)))
   ((:index :git-index :git-diff)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"variable\">~A</span>" s)))
   ((:index-file :git-index-file)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"symbol\">~A</span>" s)))))

(defparameter *numbers* "0123456789")

(define-coloring-type :webkit "WebKit (text or diff)"
  :parent :diff
  :transitions
  (((:file-from)
    ((scan "(revision ")
     (set-mode :revision
	       :until (scan #\)))))
   ((:revision)
    ((scan-any *numbers*)
     (set-mode :revision-number)))
   ((:revision :revision-number)
    ((scan #\newline)
     (set-mode :first-char-on-line)))
   ((:revision-number)
    ((scan #\))
     (set-mode :revision)))
   ((:diff-added)
    ((scan "Reviewed by NOBODY (OOPS!)")
     (set-mode :oops
	       :until (advance 1)
	       :advancing nil))))
  :formatters
  (((:revision)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"string\">~A</span>" s)))
   ((:revision-number)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<a href=\"http://trac.webkit.org/changeset/~A\" class=\"diff-link\">~:*~A</a>" s)))
   ((:index-file)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<a href=\"http://trac.webkit.org/browser/trunk/~A\" class=\"diff-link\">~:*~A</a>"
	      s)))
   ((:git-index-file)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<a href=\"http://trac.webkit.org/browser/trunk/~A\" class=\"diff-link\">~A</a>"
	      (if (> (length s) 2)
		  (subseq s 2)
		  s)
	      s)))
   ((:oops)
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"syntaxerror\">~A</span>" s)))))
