#! /usr/bin/guile \                 -*- coding: utf-8 -*-
--no-auto-compile -s
!#

;; Copyright (C) 2012 Barry Schwartz
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

(use-modules
 ((srfi srfi-1) #:select (fold drop split-at list-tabulate iota))
 ((rnrs) :version (6) #:select (assert
                                fold-left div-and-mod
                                let*-values))
 (ice-9 format))

(define (binomial-coefficients n)
  (assert (integer? n))
  (assert (<= 0 n))
  (if (zero? n)
      '(1)
      (let ((previous (binomial-coefficients (1- n))))
        (map + (cons 0 previous) (append previous '(0))))))

(define (altsigns n)
  (assert (integer? n))
  (assert (<= 0 n))
  (if (zero? n)
      '(1)
      (let* ((previous (altsigns (1- n)))
             (last (car (last-pair previous))))
        (append previous (list (* -1 last))))))

(define (binomial-coefficients-altsigns n)
  (assert (integer? n))
  (assert (<= 0 n))
  (map * (binomial-coefficients n) (altsigns n)))

(define (sbern-basis-in-mono n)
  (assert (integer? n))
  (assert (<= 0 n))
  (let ((row (lambda (i)
               (append (make-list i 0)
                       (binomial-coefficients (- n i))))))
    (list-tabulate (1+ n) row)))

(define (mono-basis-in-sbern n)
  (assert (integer? n))
  (assert (<= 0 n))
  (let ((row (lambda (i)
               (append (make-list i 0)
                       (binomial-coefficients-altsigns (- n i))))))
    (list-tabulate (1+ n) row)))

(define (sbern-basis-in-spower n)
  (assert (integer? n))
  (assert (<= 0 n))
  (let* ((q (call-with-values (lambda () (div-and-mod n 2)) +))
         (top-row
          (lambda (i) (append (make-list i 0)
                              (binomial-coefficients (- n (* i 2) 1))
                              (make-list (1+ i) 0))))
         (middle-row
          (lambda () (append (make-list q 0)
                             '(1)
                             (make-list q 0))))
         (bottom-row
          (lambda (i) (reverse (top-row (- q i 1))))))
    (if (even? n)
        (append
         (list-tabulate q top-row)
         (list (middle-row))
         (list-tabulate q bottom-row))
        (append
         (list-tabulate q top-row)
         (list-tabulate q bottom-row)))))

(define (spower-basis-in-sbern n)
  (matrix-inverse (sbern-basis-in-spower n)))

;;-------------------------------------------------------------------------

(define (matrix-inverse-by-gauss-jordan mat)
  (let ((n (length mat)))

    ;; Check that the rows are all of length @var{n}.
    (assert (fold (lambda (row truth) (and (= (length row) n)) truth) #t mat))

    (let* ((augmented-mat (map append mat (identity-matrix n)))

           (gauss-jordan-step
            (lambda (i amat)
              (let ((find-pivot
                     (lambda (k)
                       (let* ((pivot-data
                               (fold (lambda (candidate current)
                                       (if (< (cadr current) (cadr candidate))
                                           candidate
                                           current))
                                     (list k (abs (list-ref (list-ref amat k) k)))
                                     (map (lambda (r)
                                            (list r (abs (list-ref (list-ref amat r) k))))
                                          (iota (- n k 1) (1+ k)))))
                              (i-pivot (car pivot-data))
                              (pivot-size (cadr pivot-data)))
                         (if (zero? pivot-size)
                             (error "singular matrix:" mat))
                         i-pivot))))
                (let* ((p (find-pivot i))
                       (m1 (swap-list-elements amat i p))
                       (m2 (normalize-row m1 i)))
                  m2))))
                
           (inv-amat (fold gauss-jordan-step augmented-mat (iota n))))

      (map (lambda (row) (drop row n)) inv-amat))))

;; Adjust the lead coefficient of row @var{i} to equal 1.
;; Adjust all the other rows so they are zero in column @var{i}.
(define (normalize-row amat i)
  (let*-values
      (((top-rows other-rows) (split-at amat i))
       ((row) (car other-rows))
       ((bottom-rows) (cdr other-rows))
       ((prefix the-rest) (split-at row i))
       ((lead) (car the-rest))
       ((new-row) (append prefix (map (lambda (x) (/ x lead)) the-rest)))
       ((adjust-row)
        (lambda (row)
          (let*-values
              (((prefix the-rest) (split-at row i))
               ((value) (car the-rest))
               ((pivrow) (map (lambda (x) (* x value)) (drop new-row i))))
            (append prefix (map (lambda (x p) (- x p)) the-rest pivrow))))))
    (append (map adjust-row top-rows)
            (cons new-row (map adjust-row bottom-rows)))))

;;-------------------------------------------------------------------------

(define matrix-inverse matrix-inverse-by-gauss-jordan)

;;-------------------------------------------------------------------------

(define (zero-matrix n)
  (assert (< 0 n))
  (make-list n (make-list n 0)))

(define (identity-matrix n)
  (assert (< 0 n))
  (let ((ident-row (lambda (i)
                     (append (make-list i 0)
                             (cons 1 (make-list (- n i 1) 0))))))
    (list-tabulate n ident-row)))

;;-------------------------------------------------------------------------

(define (matrix-transpose mat)
  (if (null-list? (car mat))
      '()
      (let ((first-col (map car mat))
            (other-cols (map cdr mat)))
        (cons first-col (matrix-transpose other-cols)))))

;;-------------------------------------------------------------------------

(define (matrix* . rest)
  (reduce (lambda (b a) (_matrix* a b)) 1 rest))

(define (_matrix* a b)
  (if (number? a)
      (if (number? b)
          (* a b)
          (_number*matrix a b))
      (if (number? b)
          (_matrix*number a b)
          (_matrix*matrix a b))))

(define (_number*matrix a b)
  (map (lambda (row) (map (lambda (x) (* a x)) row)) b))

(define (_matrix*number a b)
  (map (lambda (row) (map (lambda (x) (* x b)) row)) a))

(define (_matrix*matrix a b)
  (let* ((na (length a))
         (ma (length (car a)))
         (nb (length b))
         (mb (length (car b))))
    (if (not (= ma nb))
        (error "matrices not conformable for multiplication:" a '* b))
    (let ((b^ (matrix-transpose b)))
      (map (lambda (row)
             (map (lambda (col) (apply + (map * row col))) b^))
           a))))

;;-------------------------------------------------------------------------

(define (matrix/ a . rest)
  (if (null-list? rest)
      (if (number? a)
          (/ a)
          (error "cannot divide by a matrix:" a))
      (fold (lambda (c b) (_matrix/ b c)) a rest)))

(define (_matrix/ a b)
  (if (number? b)
      (if (number? a)
          (/ a b)
          (_matrix/number a b))
      (error "cannot divide by a matrix:" b)))

(define (_matrix/number a b)
  (map (lambda (row) (map (lambda (x) (/ x b)) row)) a))  

;;-------------------------------------------------------------------------
    
(define (matrix+ . rest)
  (reduce (lambda (b a) (_matrix+ a b)) 0 rest))

(define (_matrix+ a b)
  (if (number? a)
      (if (number? b)
          (+ a b)
          (_number+matrix a b))
      (if (number? b)
          (_matrix+number a b)
          (_matrix+matrix a b))))

(define (_number+matrix a b)
  (if (zero? a)
      b
      (error "number and matrix not conformable for addition:" a '+ b)))

(define (_matrix+number a b)
  (if (zero? b)
      a
      (error "matrix and number not conformable for addition:" a '+ b)))

(define (_matrix+matrix a b)
  (let* ((na (length a))
         (ma (length (car a)))
         (nb (length b))
         (mb (length (car b))))
    (if (or (not (= na nb)) (not (= ma mb)))
        (error "matrices not conformable for addition:" a '+ b))
    (map (lambda (a_row b_row) (map + a_row b_row)) a b)))
    
;;-------------------------------------------------------------------------

(define (matrix- a . rest)
  (if (null-list? rest)
      (if (number? a)
          (- a)
          (map (lambda (row) (map - row)) a))
      (fold (lambda (c b) (_matrix- b c)) a rest)))

(define (_matrix- a b)
  (if (number? a)
      (if (number? b)
          (- a b)
          (_number-matrix a b))
      (if (number? b)
          (_matrix-number a b)
          (_matrix-matrix a b))))

(define (_number-matrix a b)
  (if (zero? a)
      (map (lambda (row) (map - row)) b)
      (error "number and matrix not conformable for subtraction:" a '- b)))

(define (_matrix-number a b)
  (if (zero? b)
      a
      (error "matrix and number not conformable for subtraction:" a '- b)))

(define (_matrix-matrix a b)
  (let* ((na (length a))
         (ma (length (car a)))
         (nb (length b))
         (mb (length (car b))))
    (if (or (not (= na nb)) (not (= ma mb)))
        (error "matrices not conformable for subtraction:" a '- b))
    (map (lambda (a_row b_row) (map - a_row b_row)) a b)))
    
;;-------------------------------------------------------------------------

(define (swap-list-elements lst i j)
  (if (= i j)
      lst
      (let*-values
          (((i1) (min i j))
           ((i2) (max i j))
           ((prefix after-prefix) (split-at lst i1))
           ((elem1) (car after-prefix))
           ((middle after-middle) (split-at (cdr after-prefix)
                                            (- i2 i1 1)))
           ((elem2) (car after-middle))
           ((suffix) (cdr after-middle)))
        (append prefix (cons elem2 middle) (cons elem1 suffix)))))

;;-------------------------------------------------------------------------

(define (string-specifying-double x)
  (let ((xx (inexact->exact x)))
    (cond
     ((integer? xx) (format #f "~d.0" xx))
     (else (format #f "~d.0 / ~d.0" (numerator xx) (denominator xx))))))

(define (string-specifying-row-of-doubles row)
  (fold-left
   (lambda (s x)
     (format #f "~a, ~a" s (string-specifying-double x)))
   (string-specifying-double (car row))
   (cdr row)))

(define (string-specifying-matrix-of-doubles matrix)
  (fold-left
   (lambda (s r)
     (format #f "~a,\n  ~a" s (string-specifying-row-of-doubles r)))
   (string-specifying-row-of-doubles (car matrix))
   (cdr matrix)))

(define (double-array-string stringifier name obj)
  (format #f "double ~a[] = {\n  ~a\n};\n" name (stringifier obj)))

(define (double-vector-string name obj)
  (double-array-string string-specifying-row-of-doubles name obj))

(define (double-matrix-string name obj)
  (double-array-string string-specifying-matrix-of-doubles name obj))

(define (write-double-array-sequence attributes name-prefix
                                     stringifier
                                     obj-func max-degree)
  (for-each (lambda (deg)
              (format #t "~a~a\n"
                      (if (string=? "" attributes)
                          ""
                          (string-append attributes " "))
                      (stringifier
                       (format #f "~a~d" name-prefix deg)
                       (obj-func deg))))
            (iota (1+ max-degree))))

(define (write-double-vector-sequence attributes name-prefix
                                      obj-func max-degree)
  (write-double-array-sequence attributes name-prefix
                               double-vector-string
                               obj-func max-degree))

(define (write-double-matrix-sequence attributes name-prefix
                                      obj-func max-degree)
  (write-double-array-sequence attributes name-prefix
                               double-matrix-string
                               obj-func max-degree))

;;-------------------------------------------------------------------------

(define (comma-separate lst)
  (fold-left (lambda (s item) (format #f "~a, ~a" s item))
             (car lst) (cdr lst)))

(define (write-initializer-by-degree name-prefix max-degree)
  (format #t "{ ")
  (format #t "~a"
          (comma-separate
           (list-tabulate (1+ max-degree)
                          (lambda (deg)
                            (format #f "~a~d" name-prefix deg)))))
  (format #t " }"))

;;-------------------------------------------------------------------------

(define max-degree (string->number (cadr (command-line))))

(format #t (string-append "#include <config.h>\n"
                          "#include <precomputed_polyspline_data.h>\n"
                          "\n"))

(format #t "unsigned int\n")
(format #t "polyspline_precomputed_degree_max (void)\n")
(format #t "{\n")
(format #t "  return _POLYSPLINE_PRECOMPUTED_DEGREE_MAX;\n")
(format #t "}\n\n")

(write-double-vector-sequence "static"
                              "_binomial_coefficients_"
                              binomial-coefficients
                              max-degree)
(format #t "static const double *_binomial_coefficients_data[] = ")
(write-initializer-by-degree "_binomial_coefficients_" max-degree)
(format #t ";\n\n")
(format #t "const double *\n")
(format #t "fl_precomputed_binomial_coefficients (unsigned int degree)\n")
(format #t "{\n")
(format #t "  return ((degree <= _POLYSPLINE_PRECOMPUTED_DEGREE_MAX) ?\n")
(format #t "            _binomial_coefficients_data[degree]\n")
(format #t "            : (const double *) 0);\n")
(format #t "}\n\n")

(write-double-vector-sequence "static"
                              "_binomial_coefficients_altsigns_"
                              binomial-coefficients-altsigns
                              max-degree)
(format #t "static const double *_binomial_coefficients_altsigns_data[] = ")
(write-initializer-by-degree "_binomial_coefficients_altsigns_" max-degree)
(format #t ";\n\n")
(format #t "const double *\n")
(format #t "fl_precomputed_binomial_coefficients_altsigns (unsigned int degree)\n")
(format #t "{\n")
(format #t "  return ((degree <= _POLYSPLINE_PRECOMPUTED_DEGREE_MAX) ?\n")
(format #t "            _binomial_coefficients_altsigns_data[degree]\n")
(format #t "            : (const double *) 0);\n")
(format #t "}\n\n")

(write-double-matrix-sequence "static"
                              "_sbern_basis_in_mono_"
                              sbern-basis-in-mono
                              max-degree)
(format #t "static const double *_sbern_basis_in_mono_data[] = ")
(write-initializer-by-degree "_sbern_basis_in_mono_" max-degree)
(format #t ";\n\n")
(format #t "const double *\n")
(format #t "fl_precomputed_sbern_basis_in_mono (unsigned int degree)\n")
(format #t "{\n")
(format #t "  return ((degree <= _POLYSPLINE_PRECOMPUTED_DEGREE_MAX) ?\n")
(format #t "            _sbern_basis_in_mono_data[degree]\n")
(format #t "            : (const double *) 0);\n")
(format #t "}\n\n")

(write-double-matrix-sequence "static"
                              "_mono_basis_in_sbern_"
                              mono-basis-in-sbern
                              max-degree)
(format #t "static const double *_mono_basis_in_sbern_data[] = ")
(write-initializer-by-degree "_mono_basis_in_sbern_" max-degree)
(format #t ";\n\n")
(format #t "const double *\n")
(format #t "fl_precomputed_mono_basis_in_sbern (unsigned int degree)\n")
(format #t "{\n")
(format #t "  return ((degree <= _POLYSPLINE_PRECOMPUTED_DEGREE_MAX) ?\n")
(format #t "            _mono_basis_in_sbern_data[degree]\n")
(format #t "            : (const double *) 0);\n")
(format #t "}\n\n")

(write-double-matrix-sequence "static"
                              "_sbern_basis_in_spower_"
                              sbern-basis-in-spower
                              max-degree)
(format #t "static const double *_sbern_basis_in_spower_data[] = ")
(write-initializer-by-degree "_sbern_basis_in_spower_" max-degree)
(format #t ";\n\n")
(format #t "const double *\n")
(format #t "fl_precomputed_sbern_basis_in_spower (unsigned int degree)\n")
(format #t "{\n")
(format #t "  return ((degree <= _POLYSPLINE_PRECOMPUTED_DEGREE_MAX) ?\n")
(format #t "            _sbern_basis_in_spower_data[degree]\n")
(format #t "            : (const double *) 0);\n")
(format #t "}\n\n")

(write-double-matrix-sequence "static"
                              "_spower_basis_in_sbern_"
                              spower-basis-in-sbern
                              max-degree)
(format #t "static const double *_spower_basis_in_sbern_data[] = ")
(write-initializer-by-degree "_spower_basis_in_sbern_" max-degree)
(format #t ";\n\n")
(format #t "const double *\n")
(format #t "fl_precomputed_spower_basis_in_sbern (unsigned int degree)\n")
(format #t "{\n")
(format #t "  return ((degree <= _POLYSPLINE_PRECOMPUTED_DEGREE_MAX) ?\n")
(format #t "            _spower_basis_in_sbern_data[degree]\n")
(format #t "            : (const double *) 0);\n")
(format #t "}\n\n")

;;-------------------------------------------------------------------------
