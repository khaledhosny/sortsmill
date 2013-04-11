;; -*- mode: scheme; coding: utf-8 -*-

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

(library (sortsmill fonts peg-spacing)

  (export )

  (import (rnrs)
          (except (guile) error)
          (ice-9 match))

  (define (split-peg-name name)
    (assert (string? name))
    (let ([legal-part1? (lambda (s) (or (string=? s "l") (string=? s "r")))]
          [legal-part3? (lambda (s) (or (string=? s "k") (string=? s "s")))]
          [parts (string-split name #\;)])
      (match parts
        [((? legal-part1? part1) part2) parts]
        [((? legal-part1? part1) part2 (? legal-part3? part3)) parts]
        [_ #f])))

  ) ;; end of library.
