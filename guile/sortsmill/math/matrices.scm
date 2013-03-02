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

(library (sortsmill math matrices)

  (export not-a-matrix
          rank-deficiency-exception

          zero-based
          one-based

          matrix-shape
          matrix-dimensions
          square-matrix?
          conformable-for*?
          conformable-for+?

          zero-matrix
          zero-f64matrix
          filled-matrix
          filled-f64matrix
          I-matrix
          I-f64matrix
          scalar-matrix
          scalar-f64matrix

          matrix-map
          matrix-copy

          f64vector->diagonal-f64matrix
          vector->diagonal-matrix

          vector->matrix
          row-matrix->vector

          matrix-1x1->scalar

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

          f64matrix-scaled
          exact-matrix-scaled
          integer-matrix-scaled
          number-matrix-scaled

          f64matrix*
          exact-matrix*
          integer-matrix*
          number-matrix*

          f64matrix+
          exact-matrix+
          integer-matrix+
          number-matrix+

          f64matrix-
          exact-matrix-
          integer-matrix-
          number-matrix-

          matrix-scaled
          matrix-scaled-by-division
          matrix-negate
          matrix*
          matrix/
          matrix+
          matrix-

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

          ;;; FIXME: These need documentation and regression tests.
          f64matrix-svd-solve:USV^X^=B^
          f64matrix-svd-solve:USV^X=B

          ;;; FIXME: These need documentation and regression tests.
          ;;
          ;; In the current implementation, the following linear
          ;; system solvers use the current SVD algorithm.
          f64matrix-solve:AX=B
          f64matrix-solve:XA=B

          ;; Inverse or pseudoinverse.
          f64matrix-pinv

          ;; These use different algorithms depending on what kinds of
          ;; matrix it sees. Currently it fully supports only matrices
          ;; A that are square and have fully rank. (In floating point
          ;; you might get infinities and NaNs rather than exceptions,
          ;; if the effective rank is less than full. At the moment,
          ;; this is considered a feature.)
          ;;
          ;; The present implementation uses the current SVD algorithm
          ;; (and returns an f64matrix) if all the matrix entries are
          ;; inexact and real; otherwise an LU decomposition is used.
          matrix-solve:AX=B
          matrix-solve:XA=B

          ;; Inverse of a full rank square matrix. (In floating point
          ;; you might get infinities and NaNs rather than exceptions,
          ;; if the effective rank is less than full. At the moment,
          ;; this is considered a feature.)
          inv

          mpqmat?
          pointer->mpqmat
          mpqmat->pointer
          matrix->mpqmat
          mpqmat->matrix)

  (import (sortsmill math gsl matrices)
          (sortsmill arrays)
          (sortsmill dynlink)
          (sortsmill machine)
          (sortsmill i18n)
          (sortsmill math math-constants)
          (sortsmill kwargs)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) every iota reduce take)
          (srfi :4)
          (system foreign)
          (ice-9 match)
          (ice-9 format))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_guile_sortsmill_math_matrices"))

  (define (not-a-matrix caller irritant . more-irritants)
    (if (null? more-irritants)
        (assertion-violation caller
                             (_ "expected a vector or matrix")
                             irritant)
        (apply assertion-violation caller
               (_ "expected vectors or matrices")
               irritant more-irritants)))

  (define (rank-deficiency-exception caller . irritants)
    (apply error caller (_ "rank-deficient matrix") irritants))

  ;;-----------------------------------------------------------------------

  (define zero-matrix
    (case-lambda
      [(n)   (make-array 0 `(1 ,n) `(1 ,n))]
      [(n m) (make-array 0 `(1 ,n) `(1 ,m))] ))

  (define zero-f64matrix
    (case-lambda
      [(n)   (make-typed-array 'f64 0.0 `(1 ,n) `(1 ,n))]
      [(n m) (make-typed-array 'f64 0.0 `(1 ,n) `(1 ,m))] ))

  ;; FIXME: Is there a better name for this?
  (define filled-matrix
    (case-lambda
      [(v n)   (make-array v `(1 ,n) `(1 ,n))]
      [(v n m) (make-array v `(1 ,n) `(1 ,m))] ))

  ;; FIXME: Is there a better name for this?
  (define filled-f64matrix
    (case-lambda
      [(v n)   (make-typed-array 'f64 v `(1 ,n) `(1 ,n))]
      [(v n m) (make-typed-array 'f64 v `(1 ,n) `(1 ,m))] ))

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

  (define scalar-matrix
    (case-lambda
      [(v n)   (scalar-matrix v n n)]
      [(v n m) (let* ([A (zero-matrix n m)]
                      [diag (matrix-diagonal A)])
                 (array-fill! diag v)
                 A)] ))

  (define scalar-f64matrix
    (case-lambda
      [(v n)   (scalar-f64matrix v n n)]
      [(v n m) (let* ([A (zero-f64matrix n m)]
                      [diag (matrix-diagonal A)])
                 (array-fill! diag v)
                 A)] ))

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

  (define (matrix-1x1->scalar A)
    (if (array? A)
        (match (array-shape A)
          [((lo hi))
           (if (eqv? lo hi)
               (generalized-vector-ref A lo)
               A)]
          [((lo1 hi1) (lo2 hi2))
           (if (and (eqv? lo1 hi1) (eqv? lo2 hi2))
               (array-ref A lo1 lo2)
               A)]
          [_ A])
        A))

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

  (define (square-matrix? A)
    (apply eqv? (matrix-dimensions A)))

  (define (conformable-for*? A B)
    (let ([nk (matrix-dimensions A)]
          [km (matrix-dimensions B)])
      (= (cadr nk) (car km))))

  (define (conformable-for+? A B)
    (let ([nm   (matrix-dimensions A)]
          [n^m^ (matrix-dimensions B)])
      (and (= (car nm) (car n^m^))
           (= (cadr nm) (cadr n^m^)))))

  (define (matrix-map proc A)
    (let* ([type (array-type A)]
           [shape (array-shape A)]
           [B (apply make-typed-array type *unspecified* shape)])
      (array-map! B proc A)
      B))

  (define (matrix-copy A)
    (matrix-map identity A))

  (define (f64vector->diagonal-f64matrix v)
    (let* ([v (row-matrix->vector v)]
           [n (f64vector-length v)]
           [A (zero-f64matrix n)])
      (array-map! (matrix-diagonal A) identity v)
      A))

  (define (vector->diagonal-matrix v)
    (if (f64vector? v)
        [f64vector->diagonal-f64matrix v]
        [let* ([v (row-matrix->vector v)]
               [n (generalized-vector-length v)]
               [A (zero-matrix n)])
          (array-map! (matrix-diagonal A) identity v)
          A] ))

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

  (define (matrix-negate A)
    (if (eq? (array-type A) 'f64)
        (matrix-scaled -1.0 A)
        (matrix-scaled -1 A)))

  (define (matrix-scaling-function who gsl-proc)
    (lambda (A B)
      ;; The order of the arguments can be either matrix-scalar or
      ;; scalar-matrix.
      (match (cons A B)
        [((? array? A) . (? number? B)) (gsl-proc A B)]
        [((? number? A) . (? array? B)) (gsl-proc B A)]
        [_ (assertion-violation who
                                (_ "expected a number and a matrix")
                                A B)] )))

  (define f64matrix-scaled
    (matrix-scaling-function 'f64matrix-scaled gsl:matrix-scale-f64))

  (define exact-matrix-scaled
    (matrix-scaling-function 'exact-matrix-scaled gsl:matrix-scale-mpq))

  (define integer-matrix-scaled
    (matrix-scaling-function 'integer-matrix-scaled gsl:matrix-scale-mpz))

  (define number-matrix-scaled
    (matrix-scaling-function 'number-matrix-scaled gsl:matrix-scale-scm))

  (define (f64matrix* A B)
    (gsl:gemm-f64 gsl:CblasNoTrans gsl:CblasNoTrans 1.0 A B 0.0 #f))

  (define (exact-matrix* A B)
    (gsl:gemm-mpq gsl:CblasNoTrans gsl:CblasNoTrans 1 A B 0 #f))

  (define (integer-matrix* A B)
    (gsl:gemm-mpz gsl:CblasNoTrans gsl:CblasNoTrans 1 A B 0 #f))

  (define (number-matrix* A B)
    (gsl:gemm-scm gsl:CblasNoTrans gsl:CblasNoTrans 1 A B 0 #f))

  (define f64matrix+ gsl:matrix-add-f64)
  (define exact-matrix+ gsl:matrix-add-mpq)
  (define integer-matrix+ gsl:matrix-add-mpz)
  (define number-matrix+ gsl:matrix-add-scm)

  (define f64matrix- gsl:matrix-sub-f64)
  (define exact-matrix- gsl:matrix-sub-mpq)
  (define integer-matrix- gsl:matrix-sub-mpz)
  (define number-matrix- gsl:matrix-sub-scm)

  ;;-----------------------------------------------------------------------

  (define (noncommutative-matrix-scalar-operation who f64-op exact-op
                                                  integer-op number-op)
    (lambda (M x)
      (let ([type-M (array-type M)])
        (cond
         [(and (eq? type-M 'f64) (real? x))
          (f64-op M x)]
         [(and (exact-array? M) (exact? x))
          (if (and (uniform-integer-array? M) (integer? x))
              (integer-op M x)
              (exact-op M x))]
         [else (number-op M x)] ))))

  (define (commutative-matrix-scalar-operation who f64-op exact-op
                                               integer-op number-op)
    (lambda (A B)
      ;; The order of the arguments can be either matrix-scalar or
      ;; scalar-matrix.
      (let-values ([(M x)
                    (match (cons A B)
                      [((? array? A) . (? number? B)) (values A B)]
                      [((? number? A) . (? array? B)) (values B A)]
                      [_ (assertion-violation
                          who
                          (_ "expected a number and a matrix")
                          A B)])])
        (let ([type-M (array-type M)])
          (cond
           [(and (eq? type-M 'f64) (real? x))
            (f64-op M x)]
           [(and (exact-array? M) (exact? x))
            (if (and (uniform-integer-array? M) (integer? x))
                (integer-op M x)
                (exact-op M x))]
           [else (number-op M x)] )))))

  (define (matrix-matrix-operation f64-op exact-op integer-op number-op)
    (lambda (A B)
      (let ([type-A (array-type A)]
            [type-B (array-type B)])
        (cond
         [(and (eq? type-A 'f64) (eq? type-B 'f64))
          (f64-op A B)]
         [(and (exact-array? A) (exact-array? B))
          (if (and (uniform-integer-array? A) (uniform-integer-array? B))
              (integer-op A B)
              (exact-op A B))]
         [else (number-op A B)] ))))

  ;;-----------------------------------------------------------------------

  (define matrix-scaled
    (commutative-matrix-scalar-operation 'matrix-scaled
                                         f64matrix-scaled
                                         exact-matrix-scaled
                                         integer-matrix-scaled
                                         number-matrix-scaled))

  (define matrix-scaled-by-division
    (let ([f64-op (lambda (A b)
                    (let ([A (vector->matrix (one-based A))])
                      (gsl:matrix-div-elements-f64
                       A (apply filled-f64matrix b (matrix-dimensions A)))))]
          [exact-op (lambda (A b) (matrix-scaled A (/ b)))]
          [number-op (lambda (A b)
                       (let ([A (vector->matrix (one-based A))])
                         (gsl:matrix-div-elements-scm
                          A (apply filled-matrix b (matrix-dimensions A)))))])
      (let ([integer-op exact-op])
        (noncommutative-matrix-scalar-operation 'matrix-scaled-by-division
                                                f64-op exact-op integer-op
                                                number-op))))

  ;;-----------------------------------------------------------------------

  (define multiply-matrices
    (matrix-matrix-operation f64matrix* exact-matrix* integer-matrix*
                             number-matrix*))

  (define matrix*
    (case-lambda
      [(A B)
       (cond [(array? A)
              (cond [(number? B) (matrix-scaled B A)]
                    [(array? B)  (multiply-matrices A B)]
                    [else
                     (assertion-violation
                      'matrix*
                      (_ "the second operand has illegal type") B)] )]
             [(number? A)
              (cond [(array? B) (matrix-scaled A B)]
                    [(number? B) (* A B)]
                    [else
                     (assertion-violation
                      'matrix*
                      (_ "the first operand has illegal type") A)] )] )]
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

  (define add-matrices
    (matrix-matrix-operation f64matrix+ exact-matrix+ integer-matrix+
                             number-matrix+))

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

  (define subtract-matrices
    (matrix-matrix-operation f64matrix- exact-matrix- integer-matrix-
                             number-matrix-))

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
    (cond [(not rcond)       (* 100 c:dbl-epsilon-exact)]
          [(negative? rcond) (* 100 c:dbl-epsilon-exact)]
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
         ['golub-reinsch (gsl:svd-golub-reinsch-f64 A)]
         ['modified-golub-reinsch (gsl:svd-modified-golub-reinsch-f64 A)]
         ['jacobi (gsl:svd-jacobi-f64 A)] )] ))

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

  (define/kwargs (f64matrix-solve:AX=B A B
                                       [full-rank? #t]
                                       [rcond (current-matrix-svd-rcond)])
    (call-with-values (lambda () (f64matrix-svd A))
      (lambda (U S V)
        (let ([effective-rank (matrix-svd-effective-rank S rcond)])
          (when full-rank?
            (unless (= effective-rank (f64vector-length S))
              (rank-deficiency-exception 'f64matrix-solve:AX=B A)))
          (values (gsl:svd-solve-f64 U S V B) effective-rank)))))

  (define/kwargs (f64matrix-solve:XA=B A B
                                       [full-rank? #t]
                                       [rcond (current-matrix-svd-rcond)])
    (matrix-transpose (f64matrix-solve:AX=B (matrix-transpose A)
                                            (matrix-transpose B)
                                            full-rank? rcond)))

  (define/kwargs (f64matrix-pinv A
                                 [full-rank? #t]
                                 [rcond (current-matrix-svd-rcond)])
    "Return the inverse or Moore-Penrose pseudoinverse of A, and the
effective rank of A."
    (call-with-values (lambda () (f64matrix-svd A))
      (lambda (U S V)
        (let ([effective-rank (matrix-svd-effective-rank S rcond)])
          (when full-rank?
            (unless (= effective-rank (f64vector-length S))
              (rank-deficiency-exception 'f64matrix-pinv A)))
          (let* ([S-pinv (matrix-map (lambda (x) (if (flzero? x) x (/ x)))
                                     (matrix-svd-limit-rank S effective-rank))]
                 [A-pinv (f64matrix*
                          (f64matrix* V (f64vector->diagonal-f64matrix S-pinv))
                          (matrix-transpose U))])
            (values A-pinv effective-rank))))))

  ;;-----------------------------------------------------------------------

  (define (exact-matrix-solve:AX=B A B)
    (call-with-values (lambda () (gsl:lu-decomposition-mpq-fast-pivot A))
      (lambda (LU permutation signum)
        (gsl:lu-solve-mpq LU permutation B))))

  (define (number-matrix-solve:AX=B A B)
    (call-with-values (lambda () (gsl:lu-decomposition-scm A))
      (lambda (LU permutation signum)
        (gsl:lu-solve-scm LU permutation B))))

  (define/kwargs (matrix-solve:AX=B A B)
    (assert (square-matrix? A))
    (let ([type-A (array-type A)]
          [type-B (array-type B)])
      (cond
       [(and (eq? type-A 'f64) (eq? type-B 'f64))
        (let-values ([(X effective-rank)
                      (f64matrix-solve:AX=B A B #:full-rank? #t)])
          X)]
       [(and (exact-array? A) (exact-array? B))
        (exact-matrix-solve:AX=B A B)]
       [(and (inexact-real-array? A) (inexact-real-array? B))
        (let-values ([(X effective-rank)
                      (f64matrix-solve:AX=B (matrix->f64matrix A)
                                            (matrix->f64matrix B)
                                            #:full-rank? #t)])
          X)]
       [else (number-matrix-solve:AX=B A B)] )))

  (define/kwargs (matrix-solve:XA=B A B)
    (matrix-transpose (matrix-solve:AX=B (matrix-transpose A)
                                         (matrix-transpose B))))

  ;;-----------------------------------------------------------------------

  (define (exact-matrix-invert A)
    (call-with-values (lambda () (gsl:lu-decomposition-mpq-fast-pivot A))
      (lambda (LU permutation signum)
        (gsl:lu-invert-mpq LU permutation))))

  (define (number-matrix-invert A)
    (call-with-values (lambda () (gsl:lu-decomposition-scm A))
      (lambda (LU permutation signum)
        (gsl:lu-invert-scm LU permutation))))

  (define/kwargs (inv A)
    (assert (square-matrix? A))
    (let ([type-A (array-type A)])
      (cond
       [(eq? type-A 'f64)
        (let-values ([(X effective-rank)
                      (f64matrix-pinv A #:full-rank? #t)])
          X)]
       [(exact-array? A) (exact-matrix-invert A)]
       [(inexact-real-array? A)
        (let-values ([(X effective-rank)
                      (f64matrix-pinv (matrix->f64matrix A)
                                      #:full-rank? #t)])
          X)]
       [else (number-matrix-invert A)] )))

  ;;-----------------------------------------------------------------------

  (define-wrapped-pointer-type mpqmat
    mpqmat? pointer->mpqmat mpqmat->pointer
    (lambda (matrix port)
      (format port "#<mpqmat ~s 0x~x>"
              (mpqmat->matrix matrix)
              (pointer-address (mpqmat->pointer matrix)))))

  (define (private:mpqmat? mpqmat)
    (mpqmat? mpqmat))

  (define (private:pointer->mpqmat p)
    (pointer->mpqmat p))

  (define (private:mpqmat->pointer mpqmat)
    (mpqmat->pointer mpqmat))

  ;;-----------------------------------------------------------------------

  ) ;; end of library.
