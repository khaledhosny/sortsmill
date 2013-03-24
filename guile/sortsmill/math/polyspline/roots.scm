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

(library (sortsmill math polyspline roots)

  (export poly:sign-variations-f64
          poly:sign-variations-scm

          ;; Budan’s 0_1 roots test.
          poly:budan-0_1-scm-mono

          ;; Isolate roots of a square-free polynomial in the interval
          ;; [0,1]. You want to do this with exact arithmetic. The
          ;; intervals are returned as an ordered list. Intervals of
          ;; length zero are closed; intervals of non-zero length are
          ;; open.
          poly:isolate-roots-scm-mono

          poly:find-bracketed-root-f64-mono
          poly:find-bracketed-root-f64-bern-schumaker-volk
          poly:find-bracketed-root-f64-bern-de-casteljau
          poly:find-bracketed-root-f64-sbern-schumaker-volk
          poly:find-bracketed-root-f64-sbern-de-casteljau
          poly:find-bracketed-root-f64-spower
          poly:find-bracketed-root-scm-mono-exact
          poly:find-bracketed-root-scm-bern-exact
          poly:find-bracketed-root-scm-sbern-exact
          poly:find-bracketed-root-scm-spower-exact

          poly:find-roots-0_1-scm-mono
          poly:preferred-find-roots-eval-algorithm)

  (import (sortsmill math polyspline bases)
          (sortsmill math polyspline div)
          (sortsmill math polyspline subdiv)
          (sortsmill math matrices)
          (sortsmill dynlink)
          (sortsmill kwargs)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (only (srfi :26) cut)
          (ice-9 match))

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_math_polyspline_roots"))

  (define poly:preferred-find-roots-eval-algorithm
    ;; Choices include @code{'de-casteljau} and
    ;; @code{'schumaker-volk}.
    (make-fluid 'schumaker-volk))

  (define/kwargs (poly:find-roots-0_1-scm-mono
                  poly
                  [eval-algorithm (fluid-ref poly:preferred-find-roots-eval-algorithm)])
    "Find the real roots of a univariate polynomial in monomial form,
in the closed interval [0,1]. Multiple roots are counted only
once. The roots are returned as a list of values in ascending order."
    (assert (not (identically-zero? poly)))
    (let-values ([(finder basis) (choose-finder eval-algorithm)])
      (let* ([sqfr (filter (negate identically-one) (poly:sqfr-scm-mono poly))]
             [roots (apply append
                           (map (cut solve-sqfr-poly finder basis <>) sqfr))])
        ;; Instead of the big sort below, one could do merges of the
        ;; output of
        ;;
        ;;    @code{(map solve-sqfr-poly <>)}
        ;;
        ;; because the sublists already are ordered.
        (sort roots <))))

  (define (identically-zero? poly)
    (for-all zero? (vector->list (row-matrix->vector poly))))

  (define (choose-finder algorithm)
    (match algorithm
      ['de-casteljau (values poly:find-bracketed-root-f64-bern-de-casteljau
                             'bern)]
      ['schumaker-volk (values poly:find-bracketed-root-f64-sbern-schumaker-volk
                               'sbern)]
      [_ (assertion-violation 'poly:preferred-find-roots-eval-algorithm
                              (_ "expected 'de-casteljau or 'schumaker-volk")
                              algorithm)]))

  (define (solve-sqfr-poly finder basis poly)
    (let ([intervals (poly:isolate-roots-scm-mono poly)])
      (map (cut interval->root finder basis poly <>) intervals)))

  (define (interval->root finder basis poly interval)
    (match interval
      [(a . a) a]
      [(a . b)
       (match (zero-based (row-matrix->vector poly))
         [#(v u)
          ;; The line v + ut = 0. Solve it exactly.
          (- (/ v u))]
         [_
          ;; Solve in a relatively stable way, using floating
          ;; point. Exact arithmetic and Brent’s method root-finders go
          ;; together very poorly.
          (let* ([p0_1 (poly:portion-scm-mono poly a b)]
                 [bern0_1 (poly:change-basis 'mono basis p0_1)]
                 [root_in_0_1 (finder (matrix->f64matrix bern0_1) 0.0 1.0)])
            (+ a (* (- b a) (inexact->exact root_in_0_1))))] )] ))

  (define (identically-one poly)
    (match (zero-based poly)
      [#(1) #t]
      [_ #f]))

  ) ;; end of library.
