;;;; colorize-package.lisp

(defpackage :colorize (:use :common-lisp)
            (:export :scan-string :format-scan
                     :find-coloring-type :autodetect-coloring-type
                     :coloring-types :scan :scan-any :advance :call-parent-formatter
                     :*coloring-css* :make-background-css :*css-background-class*
                     :colorize-file :colorize-file-to-stream :*version-token*))
