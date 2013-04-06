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
  
(library (sortsmill fonts contours)

  (export make-contour-point       contour-point?
          contour-point-x          contour-point-x-set!
          contour-point-y          contour-point-y-set!
          contour-point-on-curve?  contour-point-on-curve?-set!
          contour-point-selected?  contour-point-selected?-set!
          contour-point-name       contour-point-name-set!

          make-contour     contour?
          contour-points   contour-points-set!
          contour-closed?  contour-closed?-set!
          contour-degree   contour-degree-set!
          contour-name     contour-name-set!

          contour->malloced-SplinePointList
          )

  (import (sortsmill fontforge-api)
          (sortsmill dynlink)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error
                  make-record-type      ; Use R⁶RS records instead.
                  )
          (srfi :4)                     ; Uniform numeric vectors.
          (system foreign)
          (ice-9 match))

  (sortsmill-dynlink-declarations "#include <contour_interface.h>")

  (define SSFromQuadraticContourData
    ;; int SSFromQuadraticContourData (SplineSet **result,
    ;;                                 double *x_vals, double *y_vals,
    ;;                                 int8_t *on_curve_vals, int8_t *selected_vals,
    ;;                                 int pt_cnt, int is_closed,
    ;;                                 char *name, int32_t *tt_start)
    (pointer->procedure int
                        (sortsmill-dynlink-func "SSFromQuadraticContourData")
                        `(* * * * * ,int ,int * *)))

  (define SSFromCubicContourData
    ;; int SSFromCubicContourData (SplineSet **result,
    ;;                             double *x_vals, double *y_vals,
    ;;                             int8_t *on_curve_vals, int8_t *selected_vals,
    ;;                             int pt_cnt, int is_closed,
    ;;                             char *name, int32_t *tt_start)
    (pointer->procedure int
                        (sortsmill-dynlink-func "SSFromCubicContourData")
                        `(* * * * * ,int ,int * *)))

  (define (SSFromContourData result x-vals y-vals on-curve-vals selected-vals
                             pt-cnt is-closed is-quadratic name tt-start)
    "An implementation of SSFromContourData that avoids having Guile
say: ‘ERROR: In procedure make-foreign-function: args >= 10 currently
unimplemented’."
    ((if (zero? is-quadratic) SSFromCubicContourData SSFromQuadraticContourData)
     result x-vals y-vals on-curve-vals selected-vals pt-cnt is-closed
     name tt-start))

  (define ContourDataSizeFromSS
    ;; int ContourDataSizeFromSS (SplineSet *ss)
    (pointer->procedure int
                        (sortsmill-dynlink-func "ContourDataSizeFromSS")
                        '(*)))

  (define ContourDataFromSS
    ;; void ContourDataFromSS (SplineSet *ss, double *x_vals, double *y_vals,
    ;;                         int8_t *on_curve_vals, int8_t *selected_vals)
    (pointer->procedure void
                        (sortsmill-dynlink-func "ContourDataFromSS")
                        '(* * * * *)))

  (define-record-type contour-point
    (fields (mutable x)
            (mutable y)
            (mutable on-curve?)
            (mutable selected?)
            (mutable name))
    (protocol
     (lambda (new)
       (lambda* (x y #:key (on-curve? #t) (selected? #f) (name ""))
         (new x y on-curve? selected? name)))))

  (define (procedure:contour-point? obj)
    "A procedure version of @code{contour-point?}, for calls from C."
    (contour-point? obj))

  (define-record-type contour
    (fields (mutable points)
            (mutable closed?)
            (mutable degree)
            (mutable name))
    (protocol
     (lambda (new)
       (lambda* (points #:key (closed? #f) (degree 3) (name ""))
         (new points closed? degree name)))))

  (define (procedure:contour? obj)
    "A procedure version of @code{contour?}, for calls from C."
    (contour? obj))

  (define (contour->malloced-SplinePointList c)
    ;; int SSFromContourData (SplineSet **result,
    ;;                        double *x_vals, double *y_vals,
    ;;                        int8_t *on_curve_vals, int8_t *selected_vals,
    ;;                        int pt_cnt, int is_closed, int is_quadratic,
    ;;                        char *name, int32_t *tt_start)
    (assert (contour? c))
    (assert (list? (contour-points c)))
    (assert (not (zero? (length (contour-points c)))))
    (assert (boolean? (contour-closed? c)))
    (assert (let ([deg (contour-degree c)]) (or (= deg 2) (= deg 3))))
    (assert (string? (contour-name c)))
    (let ([points  (contour-points c)]
          [closed? (contour-closed? c)]
          [degree  (contour-degree c)]
          [name    (contour-name c)])
      (let ([splineset (make-bytevector (sizeof '*))]
            [x-vals    (list->f64vector (map contour-point-x points))]
            [y-vals    (list->f64vector (map contour-point-y points))]
            [on-curve  (list->s8vector  (map (compose boolean->0-or-1
                                                      contour-point-on-curve?)
                                             points))]
            [selected  (list->s8vector  (map (compose boolean->0-or-1
                                                      contour-point-selected?)
                                             points))]
            [tt-start  (make-s32vector 1)])
        (let ([errval (SSFromContourData (bytevector->pointer splineset)
                                         (bytevector->pointer x-vals)
                                         (bytevector->pointer y-vals)
                                         (bytevector->pointer on-curve)
                                         (bytevector->pointer selected)
                                         (length points)
                                         (boolean->0-or-1 closed?)
                                         (boolean->0-or-1 (= degree 2))
                                         (string->pointer name "UTF-8")
                                         (bytevector->pointer tt-start))])
          (case errval
            [(0) (values [pointer->SplinePointList
                          (dereference-pointer
                           (bytevector->pointer splineset))]
                         [s32vector-ref tt-start 0])]
            [(1) (assertion-violation 'contour->malloced-SplinePointList
                                      (_ "empty contour") c)] ; Is this possible?
            [(2) (assertion-violation 'contour->malloced-SplinePointList
                                      (_ "bad cubic") c)]
            [else (error 'contour->malloced-SplinePointList
                         (_ "unknown error") c)]) ))))

  (define (boolean->0-or-1 b?) (if b? 1 0))

  ) ;; end of library.
