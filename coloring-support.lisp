;; coloring-support.lisp

(in-package :colorize)


(defun close-dangling-parenthesis (paren-counter)
  "Emit enough </span> tags to account for missing close parenthesis in the source."
  (format nil "~{~A~}"
	  (loop for i from paren-counter downto 1
	       collect "</span></span>")))


(defmacro format-parenthesis (paren &key
				      open-parens close-parens 
				      before-paren after-paren
				      paren-counter)
  "Macro to help emmitting parenthesis.
It will result in a string wrapping the paren in appropriate <span> or </span>
tags.   The optional arguments before-paren and after-paren are 
added at the begin/end of the resulting string.

Note that paren, open-parens and close-parens are evaluated once as arguments.
However, paren-counter should be a setf'able counter and is modified.

Also the before-paren and after-paren are evalulated once, however only
AFTER the paren-counter is updated. This is done to be compatible with
the boiler plate code this replaces.
There is one case it is needed, and that is when the argument to :after-paren 
is the following expression 

  :after-paren (colorize after-paren)

In this case the after paren is colorized with the new nesting level.

This behaviour is tricky and should probably be changed."
  (alexandria:once-only (paren open-parens close-parens)
    `(or (when (member ,paren (coerce ,open-parens 'list))
	   (incf ,paren-counter)
	   (format nil "~A<span class=\"paren~A\">~C<span class=\"~A\">~A"
		   (or ,before-paren "")
		   (1+ (mod (- ,paren-counter 1) 6))
		   ,paren
		   *css-background-class*
		   (or ,after-paren "")))
	 (when (member ,paren (coerce ,close-parens 'list))
	   (decf paren-counter)
	   (if (> 0 paren-counter) 
	       (progn
		 (setf paren-counter 0)
		 (format nil "~A~C~A"
			 (or ,before-paren "")
			 ,paren
			 (or ,after-paren "")))
	       (format nil "~A</span>~C</span>~A"
		       (or ,before-paren "")
		       ,paren
		       (or ,after-paren ""))))
	 (error "Expected paren, but is neither closing paren, nor open paren."))))

