#!/bin/sh
sbcl --eval "(ql:quickload '(colorize sb-introspect cl-api))" \
     --eval "(cl-api:api-gen :colorize \"docs/colorize.html\")" \
     --eval "(progn (terpri) (sb-ext:quit))"
