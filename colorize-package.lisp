;;;; colorize-package.lisp

(defpackage :colorize
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/colorize\">Github</a>")
  (:use :common-lisp)
  (:export :scan-string :format-scan :html-colorization
           :find-coloring-type :autodetect-coloring-type
           :coloring-types :scan :scan-any :advance :call-parent-formatter
           :*coloring-css* :make-background-css :*css-background-class*
           :colorize-file :colorize-file-to-stream :*version-token*))
