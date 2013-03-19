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

(library (sortsmill math polyspline implicit)

  (export poly:bezout-matrix
          poly:implicit-equation
          poly:implicit-equation-by-degrees
          poly:pretty-print-implicit-equation)

  (import (sortsmill math multivariate-polynomials)
          (sortsmill math matrices)
          (sortsmill dynlink)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) iota list-tabulate zip)
          (ice-9 format)
          (ice-9 match))

;;;;; FIXME: Will this be needed?
  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_math_polyspline_implicit"))

  ;;----------------------------------------------------------------------

  (define (poly:bezout-matrix xspline yspline)
    "Make the Bézout matrix for implicitization of a parametric planar
Bézier spline, given in monomial basis."
    (let ([xspline (zero-based (row-matrix->vector (matrix-inexact->exact xspline)))]
          [yspline (zero-based (row-matrix->vector (matrix-inexact->exact yspline)))])
      (assert (= (vector-length xspline) (vector-length yspline)))
      (let ([deg-max (max (max-degree xspline) (max-degree yspline))])
        (let ([xspline (make-shared-array xspline list (+ deg-max 1))]
              [yspline (make-shared-array yspline list (+ deg-max 1))])
          (bezout-matrix (x-xspline xspline) (y-yspline yspline)
                         #:sum multipoly+ #:difference multipoly-
                         #:product multipoly*)))))

  (define (max-degree spline)
    (let* ([n (vector-length spline)]
           [deg (- n 1)])
      (cond [(= deg -1) 0]
            [(zero? (vector-ref spline deg))
             (max-degree (make-shared-array spline list deg))]
            [else deg])))

  (define (x-xspline xspline)
    (let* ([n (vector-length xspline)]
           [sp (make-vector n)]
           [deg0-entry (make-array 0 2 2)])
      (array-set! deg0-entry (- (vector-ref xspline 0)) 0 0)
      (array-set! deg0-entry 1 1 0)     ; Represents ‘x’.
      (vector-set! sp 0 deg0-entry)
      (do ([i 1 (+ i 1)]) ([= i n])
        (vector-set! sp i (make-array (- (vector-ref xspline i)) 1 1)))
      sp))

  (define (y-yspline yspline)
    (let* ([n (vector-length yspline)]
           [sp (make-vector n)]
           [deg0-entry (make-array 0 2 2)])
      (array-set! deg0-entry (- (vector-ref yspline 0)) 0 0)
      (array-set! deg0-entry 1 0 1)     ; Represents ‘y’.
      (vector-set! sp 0 deg0-entry)
      (do ([i 1 (+ i 1)]) ([= i n])
        (vector-set! sp i (make-array (- (vector-ref yspline i)) 1 1)))
      sp))

  (define (poly:implicit-equation matrix)
    "Given the Bézout matrix made by poly:bezout-matrix, find its
determinant (the Bézout resultant), which is the implicit equation of
the spline. Returns #f if there is no implicit equation because the
curve is degenerated to a point."
    (bezout-resultant matrix #:sum multipoly+ #:difference multipoly-
                      #:product multipoly*))

  (define (poly:implicit-equation-by-degrees eq)
    (if eq
        [begin
          (assert (apply = (matrix-dimensions eq)))
          (let ([n (car (matrix-dimensions eq))])
            (fold-left
             (lambda (prior degree)
               (cons (fold-left
                      (lambda (prior^ i)
                        (let ([j (- degree i)])
                          (cons (array-ref eq i j) prior^)))
                      '()
                      (iota (+ degree 1) 0))
                     prior))
             '()
             (iota n (- n 1) -1)))]
        #f))

  (define (poly:pretty-print-implicit-equation eq)
    (let ([eq (if (array? eq) (poly:implicit-equation-by-degrees eq) eq)])
      (if eq
          [let ([terms (apply append (map terms-for-degree eq))])
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

  ;;----------------------------------------------------------------------

  ) ;; end of library.
