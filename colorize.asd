(in-package #:cl-user)

(defpackage #:colorize-system
    (:use #:cl #:asdf))

(in-package #:colorize-system)

(defsystem :colorize
  :name "colorize"
  :description "A Syntax highlighting library"
  :author "Brian Mastenbrook"
  :maintainer "Brit Butler <redline6561@gmail.com>"
  :version "0.9"
  :licence "MIT"
  :depends-on (:html-encode :split-sequence :alexandria)
  :components ((:file "colorize-package")
               (:file "coloring-css" :depends-on ("colorize-package"))
               (:file "colorize" :depends-on ("colorize-package" "coloring-css"))
               (:file "abbrev")
               (:file "clhs-lookup" :depends-on ("abbrev"))
               (:file "r5rs-lookup")
               (:file "elisp-lookup")
               (:file "coloring-types"
                      :depends-on ("colorize" "clhs-lookup" "coloring-support"))
	       (:file "coloring-support" :depends-on ("colorize"))))
