#! /bin/env guile \ -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-
--no-auto-compile -s
!#

;; Copyright (C) 2013 Barry Schwartz
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

(import (sortsmill dynlink)
        (sortsmill strings))

(enable-hash-guillemet-strings)

(set-port-encoding! (current-output-port) "utf-8")

(write-dynlink-symbol-use-c-code
 (apply extract-dynlink-symbols-from-files (cdr (command-line))))

