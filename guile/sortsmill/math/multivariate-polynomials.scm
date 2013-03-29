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

(library (sortsmill math multivariate-polynomials)

  ;; Multivariate polynomials represented inefficiently as matrix-0based,
  ;; multidimensional, hypercubic arrays, with nearly half the entries
  ;; unused. Each array dimension corresponds to the powers of a
  ;; variable; the index of the entry equals the power.

  (export multipoly+
          multipoly-
          multipoly*

          multipoly:by-degrees
          multipoly:pretty-print)

  (import (sortsmill math matrices)
          (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) iota list-tabulate zip)
          (ice-9 match)
          (ice-9 format))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_math_multivariate_polynomials"))

  (define (multipoly:by-degrees poly)
    (if poly
        [begin
          (assert (apply = (matrix-dimensions poly)))
          (let ([n (car (matrix-dimensions poly))])
            (fold-left
             (lambda (prior degree)
               (cons (fold-left
                      (lambda (prior^ i)
                        (let ([j (- degree i)])
                          (cons (array-ref poly i j) prior^)))
                      '()
                      (iota (+ degree 1) 0))
                     prior))
             '()
             (iota n (- n 1) -1)))]
        #f))

  (define (multipoly:pretty-print poly)
    (let ([poly (if (array? poly) (multipoly:by-degrees poly) poly)])
      (if poly
          [let ([terms (apply append (map terms-for-degree poly))])
            (if (null? terms)
                "0"
                (string-join terms " + "))]
          #f)))

  (define (terms-for-degree coefs)
    (let ([p (- (length coefs) 1)])
      (map format-term
           (filter (lambda (e) (not (= (car e) 0)))
                   (zip coefs (basis-for-degree p))))))

  (define (format-term term)
    (cond
     [(string=? (cadr term) "1") (format #f "~a" (car term))]
     [(= (car term) 1) (format #f "~a" (cadr term))]
     [(= (car term) -1) (format #f "-~a" (cadr term))]
     [else (format #f "~a~a" (car term) (cadr term))] ))

  (define (basis-for-degree p)
    (case p
      [(0) '("1")]
      [else
       (let ([x-count (lambda (i) (- p i))]
             [y-count (lambda (i) i)]
             [show-x  (lambda (n)
                        (case n
                          [(0) ""]
                          [(1) "x"]
                          [else (format #f "x~a"
                                        (integer->superscript n))]))]
             [show-y  (lambda (n)
                        (case n
                          [(0) ""]
                          [(1) "y"]
                          [else (format #f "y~a"
                                        (integer->superscript n))]))])
         (list-tabulate (+ p 1)
                        (lambda (i)
                          (format #f "~a~a"
                                  (show-x (x-count i))
                                  (show-y (y-count i))))))] ))

  (define (integer->superscript n)
    (string-map char->superscript (format #f "~d" n)))

  (define (char->superscript d)
    ;; Note that to display these superscripts you will have to switch
    ;; to a compatible locale. Guile’s default locale is inadequate.
    (match d
      [#\0 #\⁰]
      [#\1 #\¹]
      [#\2 #\²]
      [#\3 #\³]
      [#\4 #\⁴]
      [#\5 #\⁵]
      [#\6 #\⁶]
      [#\7 #\⁷]
      [#\8 #\⁸]
      [#\9 #\⁹]
      [#\- #\⁻]
      [other other]))

  ) ;; end of library.
