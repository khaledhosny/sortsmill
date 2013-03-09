#!/bin/sh
# -*- scheme -*-
GUILE_AUTO_COMPILE=0
exec guile -s "${0}" ${1+"$@"}
!#

(import (sortsmill math)
        (sortsmill math polyspline eval)
        (rnrs)
        (except (guile) error)
        (only (srfi :1) iota)
        (ice-9 match))

(define f64-functions
  `((mono . ,poly:eval-f64-mono)
    (bern . ,poly:eval-f64-bern-schumaker-volk)
    (bern . ,poly:eval-f64-bern-de-casteljau)
    (sbern . ,poly:eval-f64-sbern-schumaker-volk)
    (sbern . ,poly:eval-f64-sbern-de-casteljau)
    (spower . ,poly:eval-f64-spower)))

(define scm-functions
  `((mono . ,poly:eval-scm-mono)
    (bern . ,poly:eval-scm-bern-schumaker-volk)
    (bern . ,poly:eval-scm-bern-de-casteljau)
    (sbern . ,poly:eval-scm-sbern-schumaker-volk)
    (sbern . ,poly:eval-scm-sbern-de-casteljau)
    (spower . ,poly:eval-scm-spower)))

(define test-polynomials
  `(#(0)
    #(1)
    #(10 -20)
    #(10 -20 -35)
    #(1000 -210 -3 5)
    #(1 2 3 4 5)
    #(1 -2 3 -4 5 -6)))

(define real-test-times (iota 201 -10 1/10))

(define complex-test-times (map + real-test-times
                                (reverse real-test-times)))

(define (naive-poly-eval coefs t)
  (let ([powers (map (lambda (i) (expt t i))
                     (iota (vector-length coefs)))])
    (apply + (map * (vector->list coefs) powers))))

(define (test-eval coefs transformed-coefs func t)
  (unless (<= (magnitude (- (naive-poly-eval coefs t)
                            (func transformed-coefs t)))
              1e-8)
    (assertion-violation "test_guile_polyspline_eval.scm"
                         "polynomial evaluation failed"
                         (list coefs transformed-coefs func t))))

(for-each
 (lambda (coefs)
   (for-each
    (match-lambda
     [(basis . func)
      (let ([transformed-coefs (matrix->f64matrix
                                (poly:change-basis 'mono basis coefs))])
        (for-each
         (lambda (t) (test-eval coefs transformed-coefs func t))
         real-test-times))])
    f64-functions))
 test-polynomials)

(for-each
 (lambda (coefs)
   (for-each
    (match-lambda
     [(basis . func)
      (let ([transformed-coefs (poly:change-basis 'mono basis coefs)])
        (for-each
         (lambda (t) (test-eval coefs transformed-coefs func t))
         real-test-times))])
    scm-functions))
 test-polynomials)


(for-each
 (lambda (coefs)
   (for-each
    (match-lambda
     [(basis . func)
      (let ([transformed-coefs (poly:change-basis 'mono basis coefs)])
        (for-each
         (lambda (t) (test-eval coefs transformed-coefs func t))
         complex-test-times))])
    scm-functions))
 test-polynomials)
