# colorize
colorize is a lisp library for syntax highlighting supporting the following languages:
* Common Lisp, ```:common-lisp```
* Emacs Lisp, ```:elisp```
* Scheme, ```:scheme```
* Clojure, ```:clojure``` *alpha*
* C, ```:c```
* C++, ```:c++```
* Java, ```:java```
* Python, ```:python```
* Erlang, ```:erlang```
* Haskell, ```:haskell```
* Objective-C, ```:objective-c```
* Diff, ```:diff```
* Webkit, ```:webkit```

## Install
You are strongly encouraged to use this library via [Quicklisp](http://quicklisp.org/). Simply start your lisp and run: ```(ql:quickload 'colorize)```.

## Getting Started
[The API](http://redlinernotes.com/docs/colorize.html) has three main entry points:

1. COLORIZE-FILE which takes a language keyword and input file and writes the result to an html file in the same directory. An alternate path for the output file may be provided as a third argument.
2. COLORIZE-FILE-TO-STREAM takes a keyword indicating the language, the path of a file to color, and a stream to write the result to. By default, it writes a complete self-contained page but if the :wrap keyword is given nil it only writes the colorized code snippet.
3. HTML-COLORIZATION takes a keyword indicating the language and a string of code and returns the colorized html.

In addition, the COLORING-TYPES function takes no arguments and returns a list of the supported coloring types as keywords. Finally, the variable *COLORING-CSS* returns suggested css code for highlighting the produced html.

That's all for now. And remember, patches welcome!
