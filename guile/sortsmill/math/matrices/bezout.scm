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

(library (sortsmill math matrices bezout)

  #| See http://en.wikipedia.org/wiki/Bézout_matrix |#

  (export bezout-matrix)

  (import (sortsmill math matrices base)
          (sortsmill math polyspline elev)
          (sortsmill kwargs)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error))

  (define/kwargs (bezout-matrix poly1 poly2 (sum +) (difference -) (product *))
    (let* ([poly1 (zero-based (row-matrix->vector poly1))]
           [poly2 (zero-based (row-matrix->vector poly2))]
           [n1 (vector-length poly1)]
           [n2 (vector-length poly2)])
      (assert (= n1 n2))
      (let* ([n (- n1 1)]
             [B (make-array *unspecified* `(1 ,n) `(1 ,n))])
        (do ([i 1 (+ i 1)]) ([= n1 i])
          (do ([j 1 (+ j 1)]) ([= n1 j])
            ;; The following code avoids using the zero element of the
            ;; field.
            [let ([m (min i (- n1 j))]
                  [entry (difference (product (vector-ref poly1 j)
                                              (vector-ref poly2 (- i 1)))
                                     (product (vector-ref poly1 (- i 1))
                                              (vector-ref poly2 j)))])
              (do ([k 2 (+ k 1)]) ([< m k])
                (set! entry
                      (sum entry
                           (difference (product (vector-ref poly1 (+ j k -1))
                                                (vector-ref poly2 (- i k)))
                                       (product (vector-ref poly1 (- i k))
                                                (vector-ref poly2 (+ j k -1)))))))
              (array-set! B entry i j)] ))
        B)))

  ) ;; end of library.
