;;;; coloring-css.lisp

(in-package :colorize)

(defparameter *coloring-css*
  ".symbol { color : #770055; background-color : transparent; border: 0px; margin: 0px;}
a.symbol:link { color : #229955; background-color : transparent; text-decoration: none; border: 0px; margin: 0px; }
a.symbol:active { color : #229955; background-color : transparent; text-decoration: none; border: 0px; margin: 0px; }
a.symbol:visited { color : #229955; background-color : transparent; text-decoration: none; border: 0px; margin: 0px; }
a.symbol:hover { color : #229955; background-color : transparent; text-decoration: none; border: 0px; margin: 0px; }
.special { color : #FF5000; background-color : inherit; }
.keyword { color : #770000; background-color : inherit; }
.comment { color : #007777; background-color : inherit; }
.string { color : #777777; background-color : inherit; }
.atom { color : #314F4F; background-color : inherit; }
.macro { color : #FF5000; background-color : inherit; }
.variable { color : #36648B; background-color : inherit; }
.function { color : #8B4789; background-color : inherit; }
.attribute { color : #FF5000; background-color : inherit; }
.character { color : #0055AA; background-color : inherit; }
.syntaxerror { color : #FF0000; background-color : inherit; }
.diff-deleted { color : #5F2121; background-color : inherit; }
.diff-added { color : #215F21; background-color : inherit; }
span.paren1 { background-color : inherit; -webkit-transition: background-color 0.2s linear; }
span.paren1:hover { color : inherit; background-color : #BAFFFF; }
span.paren2 { background-color : inherit; -webkit-transition: background-color 0.2s linear; }
span.paren2:hover { color : inherit; background-color : #FFCACA; }
span.paren3 { background-color : inherit; -webkit-transition: background-color 0.2s linear; }
span.paren3:hover { color : inherit; background-color : #FFFFBA; }
span.paren4 { background-color : inherit; -webkit-transition: background-color 0.2s linear; }
span.paren4:hover { color : inherit; background-color : #CACAFF; }
span.paren5 { background-color : inherit; -webkit-transition: background-color 0.2s linear; }
span.paren5:hover { color : inherit; background-color : #CAFFCA; }
span.paren6 { background-color : inherit; -webkit-transition: background-color 0.2s linear; }
span.paren6:hover { color : inherit; background-color : #FFBAFF; }
")

(defvar *css-background-class* "")

(defun for-css (thing)
  (if (symbolp thing) (string-downcase (symbol-name thing))
      thing))

(defun make-background-css (color &key (class *css-background-class*) (extra nil))
  (format nil ".~A { background-color: ~A; color: black; ~{~A; ~}}~:*~:*~:*
.~A:hover { background-color: ~A; color: black; ~{~A; ~}}~%"
          class color
          (mapcar #'(lambda (extra)
                      (format nil "~A : ~{~A ~}"
                              (for-css (first extra))
                              (mapcar #'for-css (cdr extra))))
                  extra)))
