#! /bin/env python
#| -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*- |#

#|                                          |#
#| A simple Python script written in Guile. |#
#|                                          |#

pass;import sortsmill.guile as guile
pass;guile.guile_eval_string ("""

(display "hello, world\n")

;""")
