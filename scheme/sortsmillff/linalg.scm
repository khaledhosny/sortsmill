;; -*- coding: utf-8 -*-

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;; FIXME: Write tests for this. ;;
;;                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (sortsmillff linalg)
  #:use-module (srfi srfi-1)            ; List operations.
  #:use-module (srfi srfi-11)           ; (let-values ...)
  #:use-module ((rnrs) :version (6) #:select (assert))
  #:export (matrix-invert
            matrix-invert-by-gauss-jordan
            zero-matrix
            identity-matrix
            matrix-transpose
            matrix*
            matrix/
            matrix+
            matrix-))

(define (matrix-invert-by-gauss-jordan mat)
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

(define matrix-invert matrix-invert-by-gauss-jordan)

;;-------------------------------------------------------------------------

(define (zero-matrix n)
  (make-list n (make-list n 0)))

(define (identity-matrix n)
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

;; FIXME: Put this in a module somewhere.
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
