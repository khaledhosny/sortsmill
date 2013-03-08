#!/bin/sh
# -*- scheme -*-
GUILE_AUTO_COMPILE=0
exec guile -s "${0}" ${1+"$@"}
!#

(import (sortsmill math)
        (sortsmill math polyspline bases)
        (rnrs)
        (except (guile) error)
        (only (srfi :1) iota)
        (srfi :26)
        (ice-9 match))

(define bases '(mono bern sbern spower))

;; Check inverses.
(for-each
 (lambda (basis1)
   (for-each
    (lambda (basis2)
      (for-each
       (lambda (degree)
         (let* ([mat1 (poly:basis-transformation basis1 basis2 degree)]
                [mat2 (poly:basis-transformation basis2 basis1 degree)]
                [product (matrix* mat1 mat2)])
           (unless (equal? product (I-matrix (+ degree 1)))
             (assertion-violation "test_guile_polyspline_bases.scm"
                                  "inverses test failed"
                                  (list basis1 basis2 degree)))))
       (iota 20)))
    bases))
 bases)

(define (check-matrix basis1 basis2 degree expected)
  (let ([mat (poly:basis-transformation basis1 basis2 degree)])
    (unless (equal? mat expected)
      (assertion-violation "test_guile_polyspline_bases.scm"
                           "check-matrix failed"
                           (list basis1 basis2 degree expected mat)))))

(for-each
 (lambda (degree)
   (check-matrix 'mono 'mono degree (I-matrix (+ degree 1)))
   (check-matrix 'bern 'bern degree (I-matrix (+ degree 1)))
   (check-matrix 'sbern 'sbern degree (I-matrix (+ degree 1)))
   (check-matrix 'spower 'spower degree (I-matrix (+ degree 1))))
 (iota 20))

(for-each
 (lambda (basis1)
   (for-each
    (lambda (basis2)
      (check-matrix basis1 basis2 0 #2@1@1((1))))
    bases))
 bases)

(check-matrix 'mono 'sbern 1 #2@1@1((1 1)
                                    (0 1)))

(check-matrix 'mono 'sbern 2 #2@1@1((1 2 1)
                                    (0 1 1)
                                    (0 0 1)))

(check-matrix 'mono 'sbern 3 #2@1@1((1 3 3 1)
                                    (0 1 2 1)
                                    (0 0 1 1)
                                    (0 0 0 1)))

(check-matrix 'mono 'sbern 4 #2@1@1((1 4 6 4 1)
                                    (0 1 3 3 1)
                                    (0 0 1 2 1)
                                    (0 0 0 1 1)
                                    (0 0 0 0 1)))

(check-matrix 'bern 'sbern 1 #2@1@1((1 0)
                                    (0 1)))

(check-matrix 'bern 'sbern 2 #2@1@1((1 0 0)
                                    (0 2 0)
                                    (0 0 1)))

(check-matrix 'bern 'sbern 3 #2@1@1((1 0 0 0)
                                    (0 3 0 0)
                                    (0 0 3 0)
                                    (0 0 0 1)))

(check-matrix 'bern 'sbern 4 #2@1@1((1 0 0 0 0)
                                    (0 4 0 0 0)
                                    (0 0 6 0 0)
                                    (0 0 0 4 0)
                                    (0 0 0 0 1)))

(check-matrix 'spower 'sbern 1 #2@1@1((1 0)
                                      (0 1)))

(check-matrix 'spower 'sbern 2 #2@1@1((1 1 0)
                                      (0 1 0)
                                      (0 1 1)))

(check-matrix 'spower 'sbern 3 #2@1@1((1 2 1 0)
                                      (0 1 0 0)
                                      (0 0 1 0)
                                      (0 1 2 1)))

(check-matrix 'spower 'sbern 4 #2@1@1((1 3 3 1 0)
                                      (0 1 1 0 0)
                                      (0 0 1 0 0)
                                      (0 0 1 1 0)
                                      (0 1 3 3 1)))

(check-matrix 'spower 'sbern 5 #2@1@1((1 4 6 4 1 0)
                                      (0 1 2 1 0 0)
                                      (0 0 1 0 0 0)
                                      (0 0 0 1 0 0)
                                      (0 0 1 2 1 0)
                                      (0 1 4 6 4 1)))

(check-matrix 'spower 'sbern 6 #2@1@1((1  5 10 10  5  1  0)
                                      (0  1  3  3  1  0  0)
                                      (0  0  1  1  0  0  0)
                                      (0  0  0  1  0  0  0)
                                      (0  0  0  1  1  0  0)
                                      (0  0  1  3  3  1  0)
                                      (0  1  5 10 10  5  1)))

(check-matrix 'spower 'sbern 7 #2@1@1((1  6 15 20 15  6  1  0)
                                      (0  1  4  6  4  1  0  0)
                                      (0  0  1  2  1  0  0  0)
                                      (0  0  0  1  0  0  0  0)
                                      (0  0  0  0  1  0  0  0)
                                      (0  0  0  1  2  1  0  0)
                                      (0  0  1  4  6  4  1  0)
                                      (0  1  6 15 20 15  6  1)))

(define (check-transitivity basis1 basis2 basis3 degree)
  (let ([mat12 (poly:basis-transformation basis1 basis2 degree)]
        [mat23 (poly:basis-transformation basis2 basis3 degree)]
        [mat13 (poly:basis-transformation basis1 basis3 degree)])
    (unless (equal? (matrix* mat12 mat23) mat13)
      (assertion-violation "test_guile_polyspline_bases.scm"
                           "check-transitivity failed"
                           (list basis1 basis2 basis3 degree
                                 mat12 mat23 mat13)))))

(for-each
 (lambda (basis1)
   (for-each
    (lambda (basis2)
      (for-each
       (lambda (basis3)
         (for-each
          (lambda (degree)
            (check-transitivity basis1 basis2 basis3 degree))
          (iota 20)))
       bases))
    bases))
 bases)

(define (check-basis-vectors basis1 basis2 degree)
  (let* ([basis-vector-matrix (I-matrix (+ degree 1))]
         [basis-vectors (map (cut matrix-row basis-vector-matrix <>)
                             (iota (+ degree 1) 1))]
         [transformed-matrix (poly:change-basis basis1 basis2
                                                basis-vector-matrix)]
         [transformed-vectors (map
                               (cut poly:change-basis basis1 basis2 <>)
                               basis-vectors)]
         [vectors-equal? (for-all equal?
                                  (map matrix->vector transformed-vectors)
                                  (map (cut matrix-row transformed-matrix <>)
                                       (iota (+ degree 1) 1)))])
    (write vectors-equal?)))
