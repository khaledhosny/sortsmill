;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2012, 2013 Barry Schwartz
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

(library (sortsmill matrices)

  (export not-a-matrix

          zero-based
          one-based

          matrix-shape
          matrix-dimensions
          conformable-for*?
          conformable-for+?

          zero-matrix
          zero-f64matrix
          I-matrix
          I-f64matrix

          vector->matrix
          row-matrix->vector

          matrix-row
          matrix-column-transpose
          matrix-column
          matrix-diagonal
          matrix-transpose

          matrix-exact->inexact
          matrix-inexact->exact

          ;; matrix->f64matrix is the same as matrix-exact->inexact.
          matrix->f64matrix

          ;; f64matrix->matrix just changes the type tag, without
          ;; converting from inexact to exact numbers.
          f64matrix->matrix

          f64matrix-f64matrix*
          f64matrix-f64matrix+
          f64matrix-f64matrix-
          f64matrix*
          f64matrix+
          f64matrix-

          matrix-scaled
          matrix-scaled-by-division
          matrix-negate
          matrix*
          matrix/
          matrix+
          matrix-

          ;; ‘Thin’ singular value decomposition algorithms from
          ;; GSL. These procedures return U, S, V, respectively, as
          ;; multiple return values.
          ;;
          ;; Golub-Reinsch requires linear workspace.
          ;;
          ;; Modified Golub-Reinsch is faster than Golub-Reinsch if
          ;; there are many more rows than columns in the input
          ;; matrix, but requires quadratic workspace.
          ;;
          ;; The Jacobi method can compute singular values to higher
          ;; relative accuracy than Golub-Reinsch, but is slower. It
          ;; uses one-sided Jacobi orthogonalization.
          f64matrix-svd-golub-reinsch
          f64matrix-svd-modified-golub-reinsch
          f64matrix-svd-jacobi

          ;; SVD algorithms enum:
          ;;   'golub-reinsch
          ;;   'modified-golub-reinsch
          ;;   'jacobi
          matrix-svd-algorithms

          ;; Initially set to 'golub-reinsch
          current-matrix-svd-algorithm-fluid
          current-matrix-svd-algorithm
          set-current-matrix-svd-algorithm!
          with-matrix-svd-algorithm

          ;; (f64matrix-svd A algorithm)
          ;;
          ;; (f64matrix-svd A) is equivalent to
          ;;   (f64matrix-svd A 'golub-reinsch)
          f64matrix-svd

          ;; RCOND value for determining the effective rank in some
          ;; SVD-based calculations. If you have LAPACK installed then
          ;; see, for example, ‘man dgelss’, for a description of
          ;; RCOND.
          current-matrix-svd-rcond-fluid
          current-matrix-svd-rcond
          set-current-matrix-svd-rcond!
          with-matrix-svd-rcond

          matrix-svd-effective-rank
          matrix-svd-limit-rank

          ;;;;;;;;;;;; FIXME FIXME FIXME FIXME
          ;;;;;;;;;;;;
          ;;;;;;;;;;;; Document that the linear systems solvers below
          ;;;;;;;;;;;; can handle multiple vectors at once, or in
          ;;;;;;;;;;;; other words that they can take a matrix B (not
          ;;;;;;;;;;;; necessarily a vector) and return a matrix X of
          ;;;;;;;;;;;; the same shape.
          ;;;;;;;;;;;;
          ;;;;;;;;;;;; Also change their names so it is clearer what
          ;;;;;;;;;;;; systems they solve, and devise similar
          ;;;;;;;;;;;; procedures for solving the system with all
          ;;;;;;;;;;;; matrices transposed and the order of
          ;;;;;;;;;;;; multiplication reversed. (That is, the same
          ;;;;;;;;;;;; problem formulated with row vectors instead of
          ;;;;;;;;;;;; column vectors for b and x.)
          ;;;;;;;;;;;;
          ;;;;;;;;;;;; Also try to move documentation into doc
          ;;;;;;;;;;;; strings.
          ;;;;;;;;;;;;
          ;;;;;;;;;;;; Also test these procedures. They really could
          ;;;;;;;;;;;; use regression tests.
          ;;;;;;;;;;;;
          ;;;;;;;;;;;; FIXME FIXME FIXME FIXME

          ;; (f64matrix-svd-solve-transposed U S V b-transpose)
          ;; returns the transpose of the (least squares) solution
          ;; vector of Ax=b, where b-transpose is a row vector.
          f64matrix-svd-solve:USV^X^=B^

          ;; (f64matrix-svd-solve U S V b) returns the (least squares)
          ;; solution vector of Ax=b, where b is a column vector.
          f64matrix-svd-solve:USV^X=B

          ;; (f64matrix-solve-transposed A b-transpose) returns the
          ;; transpose of the (least squares) solution vector of Ax=b,
          ;; where b-transpose is a row vector/matrix. In the current
          ;; implementation, the solution is computed using the
          ;; current SVG algorithm.
          f64matrix-solve:AX^=B^

          ;; (f64matrix-svd-solve A b) returns the (least squares)
          ;; solution vector of Ax=b, where b is a column matrix. In
          ;; the current implementation, the solution is computed
          ;; using the current SVG algorithm.
          f64matrix-solve:AX=B
          )

  (import (sortsmill dynlink)
          (sortsmill i18n)
          (sortsmill math-constants)
          (sortsmill kwargs)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) iota reduce take)
          (srfi :4)
          (ice-9 match))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_matrices"))

  (define (not-a-matrix caller irritant . more-irritants)
    (if (null? more-irritants)
        (assertion-violation caller
                             (_ "expected a vector or matrix")
                             irritant)
        (apply assertion-violation caller
               (_ "expected vectors or matrices")
               irritant more-irritants)))

  ;;-----------------------------------------------------------------------

  (define zero-matrix
    (case-lambda
      [(n)   (make-array 0 `(1 ,n) `(1 ,n))]
      [(n m) (make-array 0 `(1 ,n) `(1 ,m))] ))

  (define zero-f64matrix
    (case-lambda
      [(n)   (make-typed-array 'f64 0.0 `(1 ,n) `(1 ,n))]
      [(n m) (make-typed-array 'f64 0.0 `(1 ,n) `(1 ,m))] ))

  (define I-matrix
    (case-lambda
      [(n)   (I-matrix n n)]
      [(n m) (let* ([I (zero-matrix n m)]
                    [diag (matrix-diagonal I)])
               (array-fill! diag 1)
               I)] ))

  (define I-f64matrix
    (case-lambda
      [(n)   (I-f64matrix n n)]
      [(n m) (let* ([I (zero-f64matrix n m)]
                    [diag (matrix-diagonal I)])
               (array-fill! diag 1.0)
               I)] ))

  ;;-----------------------------------------------------------------------

  (define (zero-based A)
    (match (array-shape A)
      [[(lo hi)]
       (make-shared-array A
                          (lambda (i) `[,(+ i lo)])
                          `[0 ,(- hi lo)] )]
      [[(lo1 hi1) (lo2 hi2)]
       (make-shared-array A
                          (lambda (i j) `[,(+ i lo1) ,(+ j lo2)])
                          `[0 ,(- hi1 lo1)] `[0 ,(- hi2 lo2)] )]
      [_ (not-a-matrix 'zero-based A)]))

  (define (one-based A)
    (match (array-shape A)
      [[(lo hi)]
       (make-shared-array A
                          (lambda (i) `[,(+ i lo -1)])
                          `[1 ,(- hi lo -1)] )]
      [[(lo1 hi1) (lo2 hi2)]
       (make-shared-array A
                          (lambda (i j) `[,(+ i lo1 -1) ,(+ j lo2 -1)])
                          `[1 ,(- hi1 lo1 -1)] `[1 ,(- hi2 lo2 -1)] )]
      [_ (not-a-matrix 'one-based A)]))

  (define (vector->matrix v)
    (match (array-rank v)
      [1
       (match (array-shape v)
         [((lo hi))
          (make-shared-array v (lambda (i j) `[,j]) `[,lo ,lo] `[,lo ,hi])])]
      [2 v]
      [_ (not-a-matrix 'vector->matrix v)] ))

  (define (row-matrix->vector V)
    (match (array-shape V)
      [[(lo1 hi1) (lo2 hi2)]
       (if (= lo1 hi1)
           (make-shared-array V (lambda (j) `[,lo1 ,j]) `[,lo2 ,hi2])
           (assertion-violation
            'row-matrix->vector (_ "not a row matrix") V))]
      [[(_ _)] V]
      [_ (not-a-matrix 'row-matrix->vector V)] ))

  (define (matrix-shape A)
    (array-shape (vector->matrix A)))

  (define (matrix-dimensions A)
    (map cadr (matrix-shape (one-based A))))

  (define (conformable-for*? A B)
    (let ([nk (matrix-dimensions A)]
          [km (matrix-dimensions B)])
      (= (cadr nk) (car km))))

  (define (conformable-for+? A B)
    (let ([nm   (matrix-dimensions A)]
          [n^m^ (matrix-dimensions B)])
      (and (= (car nm) (car n^m^))
           (= (cadr nm) (cadr n^m^)))))

  ;;-----------------------------------------------------------------------

  (define (matrix-row A i)
    "Return a view of a matrix row as a row vector (in the form of
a Guile vector)"
    (let* ([A (vector->matrix A)]
           [shape (array-shape A)])
      (match shape
        [(_ (lo hi)) (make-shared-array A (lambda (j) `[,i ,j]) `[,lo ,hi])]
        [_ (not-a-matrix 'matrix-row A)] )))

  (define (matrix-column-transpose A j)
    "Return a view of a matrix column as a row vector (in the form of
a Guile vector)."
    (let* ([A (vector->matrix A)]
           [shape (array-shape A)])
      (match shape
        [((lo hi) _) (make-shared-array A (lambda (i) `[,i ,j]) `[,lo ,hi])]
        [_ (not-a-matrix 'matrix-column-transpose A)] )))

  (define (matrix-column A j)
    "Return a view of a matrix column as a matrix (that is, a rank-2
array)."
    (matrix-transpose (matrix-column-transpose A j)))

  (define (matrix-diagonal A)
    (let ([n (apply min (matrix-dimensions A))])
      (make-shared-array A (lambda (i) `[,i ,i]) `[1 ,n])))

  (define (matrix-transpose A)
    (transpose-array (vector->matrix A) 1 0))

  ;;-----------------------------------------------------------------------

  (define (matrix-exact->inexact A)
    (let ([B (apply make-typed-array 'f64 *unspecified* (array-shape A))])
      (array-map! B exact->inexact A)
      B))

  (define (matrix-inexact->exact A)
    (let ([B (apply make-array *unspecified* (array-shape A))])
      (array-map! B inexact->exact A)
      B))

  (define matrix->f64matrix matrix-exact->inexact)

  (define (f64matrix->matrix A)
    (let ([B (apply make-array *unspecified* (array-shape A))])
      (array-map! B identity A)
      B))

  ;;-----------------------------------------------------------------------

  (define (matrix-scaled a B)
    (if (and (real? a) (eq? (array-type B) 'f64))
        [let ([C (apply make-typed-array 'f64 *unspecified* (array-shape B))])
          (array-map! C (lambda (x) (* a x)) B)
          C]
        [let ([C (apply make-array *unspecified* (array-shape B))])
          (array-map! C (lambda (x) (* a x)) B)
          C] ))

  (define (matrix-scaled-by-division A b)
    (if (and (real? b) (eq? (array-type A) 'f64))
        [let ([C (apply make-typed-array 'f64 *unspecified* (array-shape A))])
          (array-map! C (lambda (x) (/ x b)) A)
          C]
        [let ([C (apply make-array *unspecified* (array-shape A))])
          (array-map! C (lambda (x) (/ x b)) A)
          C] ))

  (define (matrix-negate A)
    (if (eq? (array-type A) 'f64)
        (matrix-scaled -1.0 A)
        (matrix-scaled -1 A)))

  (define (f64matrix* A B)
    (f64matrix-f64matrix* (vector->matrix A) (vector->matrix B)))

  (define (f64matrix+ A B)
    (f64matrix-f64matrix+ (vector->matrix A) (vector->matrix B)))

  (define (f64matrix- A B)
    (f64matrix-f64matrix- (vector->matrix A) (vector->matrix B)))

  ;;-----------------------------------------------------------------------

  (define (row*col row column-transposed)
    (apply + (map * (generalized-vector->list row)
                  (generalized-vector->list column-transposed))))

  (define (multiplication-result-type type-A type-B)
    (match (cons type-A type-B)
      [('f64 . _) 'f64]
      [(_ . 'f64) 'f64]
      [_            #t] ))

  (define (multiply-matrices A B)
    (let ([type-A (array-type A)]
          [type-B (array-type B)])
      (match (cons type-A type-B)
        [('f64 . 'f64) (f64matrix* A B)]
        [_
         (let ([A (one-based A)]
               [B (one-based B)]
               [nk (matrix-dimensions A)]
               [km (matrix-dimensions B)])
           (unless (= (cadr nk) (car km))
             (assertion-violation
              'matrix*
              (_ "the matrices are not conformable for multiplication")
              A B))
           (let* ([n (car nk)]
                  [k (car km)]
                  [m (cadr km)]
                  [type-C (multiplication-result-type type-A type-B)]
                  [C (make-typed-array type-C *unspecified* `[1 ,n] `[1 ,m])]
                  [row-indices (iota n 1)]
                  [rows (vector-map (lambda (i) (matrix-row A i))
                                    (list->vector row-indices))]
                  [col-indices (iota m 1)]
                  [cols (vector-map
                         (lambda (j) (matrix-column-transpose B j))
                         (list->vector col-indices))])
             (for-each
              (lambda (i)
                (for-each
                 (lambda (j)
                   (array-set! C (row*col (vector-ref rows (1- i))
                                          (vector-ref cols (1- j)))
                               i j))
                 col-indices))
              row-indices)
             C))] )))

  (define matrix*
    (case-lambda
      [(A B)
       (cond [(array? A)
              (cond [(number? B) (matrix-scaled B A)]
                    [(array? B) (multiply-matrices A B)]
                    [else
                     (assertion-violation
                      'matrix*
                      (_ "the second operand has illegal type") B)])]
             [(number? A)
              (cond [(array? B) (matrix-scaled A B)]
                    [(number? B) (* A B)]
                    [else
                     (assertion-violation
                      'matrix*
                      (_ "the first operand has illegal type") A)])])]
      [(. rest) (reduce (lambda (B A) (matrix* A B)) 1 rest)] ))

  ;;-----------------------------------------------------------------------

  (define matrix/
    (case-lambda
      [(A B) (cond
              [(not (number? B))
               (assertion-violation
                'matrix/
                (if (array? B)
                    (_ "division by a matrix")
                    (_ "the second operand has illegal type"))
                B)]
              [(array? A) (matrix-scaled-by-division A B)]
              [(number? A) (/ A B)]
              [_ (assertion-violation
                  'matrix/ (_ "the first operand has illegal type") A)] )]
      [(A) (cond
            [(number? A) (/ A)]
            [(array? A) (assertion-violation
                         'matrix/ (_ "division by a matrix") A)]
            [_ (assertion-violation
                'matrix/ (_ "the operand has illegal type") A)] )]
      [(A . rest) (fold-left matrix/ A rest)] ))

  ;;-----------------------------------------------------------------------

  (define (add-matrices A B)
    (match (cons (array-type A) (array-type B))
      [('f64 . 'f64) (f64matrix+ A B)]
      [_
       (let ([A (vector->matrix (one-based A))]
             [B (vector->matrix (one-based B))]
             [nm (matrix-dimensions A)]
             [n^m^ (matrix-dimensions B)])
         (unless (and (= (car nm) (car n^m^))
                      (= (cadr nm) (cadr n^m^)))
           (assertion-violation
            'matrix+
            (_ "the matrices are not conformable for addition")
            A B))
         (let* ([n (car nm)]
                [m (cadr nm)]
                [C (make-array *unspecified* `[1 ,n] `[1 ,m])]
                [row-indices (iota n 1)]
                [col-indices (iota m 1)])
           (for-each
            (lambda (i)
              (for-each
               (lambda (j)
                 (array-set! C (+ (array-ref A i j)
                                  (array-ref B i j))
                             i j))
               col-indices))
            row-indices)
           C))] ))

  (define matrix+
    (case-lambda
      [(A B)
       (cond
        [(array? A)
         (cond
          [(array? B) (add-matrices A B)]
          [(number? B)
           (if (zero? B) A
               (assertion-violation
                'matrix+
                (_ "numbers other than zero cannot be added to matrices")
                B))] )]
        [(number? A)
         (cond
          [(array? B)
           (if (zero? A) B
               (assertion-violation
                'matrix+
                (_ "numbers other than zero cannot be added to matrices")
                A))]
          [(number? B) (+ A B)]
          [else
           (assertion-violation
            'matrix+
            (_ "the second operand has illegal type") B)] )] )]
      [(. rest) (reduce (lambda (B A) (matrix+ A B)) 0 rest)] ))

  ;;-----------------------------------------------------------------------

  (define (subtract-matrices A B)
    (match (cons (array-type A) (array-type B))
      [('f64 . 'f64) (f64matrix- A B)]
      [_
       (let ([A (vector->matrix (one-based A))]
             [B (vector->matrix (one-based B))]
             [nm (matrix-dimensions A)]
             [n^m^ (matrix-dimensions B)])
         (unless (and (= (car nm) (car n^m^))
                      (= (cadr nm) (cadr n^m^)))
           (assertion-violation
            'matrix+
            (_ "the matrices are not conformable for subtraction")
            A B))
         (let* ([n (car nm)]
                [m (cadr nm)]
                [C (make-array *unspecified* `[1 ,n] `[1 ,m])]
                [row-indices (iota n 1)]
                [col-indices (iota m 1)])
           (for-each
            (lambda (i)
              (for-each
               (lambda (j)
                 (array-set! C (- (array-ref A i j)
                                  (array-ref B i j))
                             i j))
               col-indices))
            row-indices)
           C))] ))

  (define matrix-
    (case-lambda
      [(A B)
       (cond
        [(array? A)
         (cond
          [(array? B) (subtract-matrices A B)]
          [(number? B)
           (if (zero? B) A
               (assertion-violation
                'matrix-
                (_ "numbers other than zero cannot be subtracted from matrices")
                B))] )]
        [(number? A)
         (cond
          [(array? B)
           (if (zero? A) (matrix-negate B)
               (assertion-violation
                'matrix-
                (_ "matrices cannot be subtracted from numbers other than zero")
                A))]
          [(number? B) (- A B)]
          [else
           (assertion-violation
            'matrix-
            (_ "the second operand has illegal type") B)] )] )]
      [(A) (if (number? A) (- A) (matrix-negate A))]
      [(A . rest) (fold-left matrix- A rest)] ))

  ;;-----------------------------------------------------------------------

  (define matrix-svd-algorithms
    (make-enumeration '(golub-reinsch modified-golub-reinsch jacobi)))

  (define current-matrix-svd-algorithm-fluid
    (make-fluid 'golub-reinsch))

  (define (current-matrix-svd-algorithm)
    (fluid-ref current-matrix-svd-algorithm-fluid))

  (define (set-current-matrix-svd-algorithm! algorithm)
    (assert (symbol? algorithm))
    (assert (enum-set-member? algorithm matrix-svd-algorithms))
    (fluid-set! current-matrix-svd-algorithm-fluid algorithm))

  (define-syntax with-matrix-svd-algorithm
    (lambda (x)
      (syntax-case x ()
        [(_ algorithm body body* ...)
         #'(begin
             (assert (symbol? algorithm))
             (assert (enum-set-member? algorithm matrix-svd-algorithms))
             (with-fluid* current-matrix-svd-algorithm-fluid algorithm
                          (lambda () body body* ...)))] )))

  (define (svd-rcond-value rcond)
    (cond [(not rcond)       (* 100 c-dbl-epsilon-exact)]
          [(negative? rcond) (* 100 c-dbl-epsilon-exact)]
          [else              (inexact->exact rcond)] ))

  (define current-matrix-svd-rcond-fluid
    (make-fluid (svd-rcond-value #f)))

  (define (current-matrix-svd-rcond)
    (fluid-ref current-matrix-svd-rcond-fluid))

  (define (set-current-matrix-svd-rcond! rcond)    
    (fluid-set! current-matrix-svd-rcond-fluid
                (svd-rcond-value rcond)))

  (define-syntax with-matrix-svd-rcond
    (lambda (x)
      (syntax-case x ()
        [(_ rcond body body* ...)
         #'(with-fluid*
               current-matrix-svd-rcond-fluid (svd-rcond-value rcond)
               (lambda () body body* ...))] )))

  (define f64matrix-svd
    (case-lambda
      [(A) (f64matrix-svd A (current-matrix-svd-algorithm))]
      [(A algorithm)
       (assert (symbol? algorithm))
       (assert (enum-set-member? algorithm matrix-svd-algorithms))
       (match algorithm
         ['golub-reinsch (f64matrix-svd-golub-reinsch A)]
         ['modified-golub-reinsch (f64matrix-svd-modified-golub-reinsch A)]
         ['jacobi (f64matrix-svd-jacobi A)] )] ))

  (define matrix-svd-effective-rank
    (case-lambda
      [(S) (matrix-svd-effective-rank S (current-matrix-svd-rcond))]
      [(S rcond)
       (let* ([S (zero-based (row-matrix->vector S))]
              [rcond^ (* rcond (generalized-vector-ref S 0))])
         (if (zero? rcond^) 0
             (let ([n (generalized-vector-length S)])
               (letrec
                   ([count
                     (lambda (i)
                       (cond [(= i n) n]
                             [(<= (generalized-vector-ref S i) rcond^) i]
                             [else (count (+ i 1))] ))])
                 (count 1)))))] ))

  (define (matrix-svd-limit-rank S rank)
    (let* ([S^ (row-matrix->vector S)]
           [n (generalized-vector-length S^)]
           [lst (generalized-vector->list S^)]
           [lst^ (append (take lst rank) (make-list (- n rank) 0))])
      (list->typed-array (array-type S) (array-shape S^) lst^)))

  (define/kwargs (f64matrix-svd-solve:USV^X^=B^ U S V B)
    (let* ([shape (array-shape B)]
           [X (apply make-typed-array 'f64 *unspecified* shape)])
      (match shape
        [((_ _))
         (private:f64matrix-svd-solve-vector U S V X B)
         X]
        [((lo hi) _)
         (for-each
          (lambda (i)
            (let ([xi (row-matrix->vector (matrix-row X i))]
                  [bi (row-matrix->vector (matrix-row B i))])
              (private:f64matrix-svd-solve-vector U S V xi bi)))
          (iota (- hi lo -1) lo))
         X] )))

  (define/kwargs (f64matrix-svd-solve:USV^X=B U S V B)
    (matrix-transpose
     (f64matrix-svd-solve:USV^X^=B^ U S V (matrix-transpose B))))

  (define/kwargs (f64matrix-solve:AX^=B^ A B)
    (call-with-values (lambda () (f64matrix-svd A))
      (lambda (U S V)
        (let* ([effective-rank (matrix-svd-effective-rank S)]
               [revised-S (matrix-svd-limit-rank S effective-rank)]
               [X (f64matrix-svd-solve:USV^X^=B^ U revised-S V B)])
          (values X effective-rank)))))

  (define/kwargs (f64matrix-solve:AX=B A B)
    (call-with-values (lambda () (f64matrix-svd A))
      (lambda (U S V)
        (let* ([effective-rank (matrix-svd-effective-rank S)]
               [revised-S (matrix-svd-limit-rank S effective-rank)]
               [X (f64matrix-svd-solve:USV^X=B U revised-S V B)])
          (values X effective-rank)))))

  ;;-----------------------------------------------------------------------

  ) ;; end of library.
