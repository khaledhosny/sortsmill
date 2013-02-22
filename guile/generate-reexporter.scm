#! /usr/bin/guile \ 	-*- mode: scheme; coding: utf-8 -*-
--no-auto-compile -s
!#

(import (ff-internal reexporters)
        (sortsmill hash-guillemet)
        (rnrs)
        (ice-9 match))

(enable-hash-guillemet-strings)

(set-port-encoding! (current-output-port) "utf-8")

(match (command-line)
  [(_ reexporter-name library-names file-names)
     (pretty-print-reexporter
      (call-with-input-string reexporter-name read)
      (call-with-input-string (string-append "(" library-names ")") read)
      (call-with-input-string (string-append "(" file-names ")")  read))])
