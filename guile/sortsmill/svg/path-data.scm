;; -*- mode: scheme; coding: utf-8 -*-

;; Copyright (C) 2013 Khaled Hosny and Barry Schwartz
;; This file is part of the Sorts Mill Tools.
;; 
;; Sorts Mill Tools is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; Sorts Mill Tools is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

(library (sortsmill svg path-data)

  (export
   ;; (svg:parse-path-data string #:stage 'parse-only) → list
   ;;
   ;; A top-down parser for SVG path data attributes, based closely on
   ;; the grammar given in section 8.3.9 of the SVG 1.1
   ;; specification. Outputs a list of simple commands (commands
   ;; without argument sequences). For instance:
   ;;
   ;;   "m 1000,500 c 0,276.14237 -223.85763,500 -500,500
   ;;      C 223.85763,1000 0,776.14237 0,500 0,223.85763 223.85763,0 500,0
   ;;      c 276.14237,0 500,223.85763 500,500 z"
   ;;
   ;; yields (to within roundoff)
   ;;
   ;;   ((#\m 1000 500)
   ;;    (#\c (0 276.14237) (-223.85763 500) (-500 500))
   ;;    (#\C (223.85763 1000) (0 776.14237) (0 500))
   ;;    (#\C (0 223.85763) (223.85763 0) (500 0))
   ;;    (#\c (276.14237 0) (500 223.85763) (500 500))
   ;;    (#\z))
   ;;
   ;; FIXME: Document these ----------------
   ;; (svg:parse-path-data string #:stage 'parse-only) → list
   ;; (svg:parse-path-data string #:stage 'subpath-lists) → list
   ;; (svg:parse-path-data string #:stage 'subpath-vectors) → vector
   ;; (svg:parse-path-data string #:stage 'absolute-coords) → vector
   ;; (svg:parse-path-data string #:stage 'quadratic) → vector
   ;; (svg:parse-path-data string #:stage 'cubic) → vector
   ;;
   ;; (svg:parse-path-data string) → vector  [equiv. to #:stage 'absolute-coords]
   ;;
   svg:parse-path-data

   ;; (svg:path-data-subpaths list) → list-of-lists
   ;;
   ;; Break the output of svg:parse-path-data into subpaths.
   svg:path-data-subpaths

   ;; (svg:subpaths->vectors list-or-vector) → vector-of-vectors
   ;; (svg:subpaths->lists list-or-vector) → list-of-lists
   svg:subpaths->vectors
   svg:subpaths->lists

   svg:make-subpath-vectors-absolute!

   svg:elliptic-arc-quadratic-spline-count-max
   svg:elliptic-arc-cubic-spline-count-max
   svg:elliptic-arc-quadratic-error-threshold
   svg:elliptic-arc-cubic-error-threshold

   svg:subpaths-quadratic->cubic!
   svg:expand-elliptic-arcs!
   )

  (import (sortsmill core)
          (sortsmill math polyspline)
          (sortsmill math matrices)
          (sortsmill math geometry)
          (sortsmill nearness)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) iota)
          (only (srfi :26) cut)
          (ice-9 match))

  ;;-------------------------------------------------------------------------

  (define (match-char-set cs s i)
    (if (< i (string-length s))
        (if (char-set-contains? cs (string-ref s i)) (+ i 1) #f)
        #f))

  (define (match-char-set<?> cs s i)
    (if (< i (string-length s))
        (if (char-set-contains? cs (string-ref s i)) (+ i 1) i)
        i))

  (define (match-char-set<*> cs s i)
    (if (< i (string-length s))
        (let ([j (string-skip s cs i)])
          (if j j (string-length s)))
        i))

  (define (match-char-set<+> cs s i)
    (let ([j (match-char-set cs s i)])
      (if j (match-char-set<*> cs s j) #f)))

  (define (char-set-matcher cs) (cut match-char-set cs <> <>))
  (define (char-set<?>-matcher cs) (cut match-char-set<?> cs <> <>))
  (define (char-set<*>-matcher cs) (cut match-char-set<*> cs <> <>))
  (define (char-set<+>-matcher cs) (cut match-char-set<+> cs <> <>))

  (define match<comma> (char-set-matcher (char-set #\,)))
  (define match<comma?> (char-set<?>-matcher (char-set #\,)))

  (define match<period> (char-set-matcher (char-set #\.)))

  (define match<sign?> (char-set<?>-matcher (char-set #\+ #\-)))

  (define char-set::wsp (char-set #\space #\tab #\return #\newline))
  (define match<wsp*> (char-set<*>-matcher char-set::wsp))
  (define match<wsp+> (char-set<+>-matcher char-set::wsp))

  (define char-set::digit (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (define match<digit*> (char-set<*>-matcher char-set::digit))
  (define match<digit+> (char-set<+>-matcher char-set::digit))

  (define (match<flag> s i)
    ;; Accept numbers other than 0 and 1, as suggested in SVG 1.1,
    ;; appendix F.6.2.
    (let-values ([(j v) (match<number> s i)])
      (if j
          (values j (not (zero? v)))
          (values #f #f))))

  (define (match<comma-wsp> s i)
    (let ([j1 (match<wsp+> s i)])
      (if j1
          (let ([j2 (match<comma?> s j1)])
            (match<wsp*> s j2))
          (let ([j2 (match<comma> s i)])
            (if j2
                (match<wsp*> s j2)
                #f)))))

  (define (match<comma-wsp?> s i)
    (let ([j (match<comma-wsp> s i)])
      (if j j i)))

  (define match<digit-sequence> match<digit+>)
  (define match<digit-sequence?> match<digit*>)

  (define match<exponent>
    (let ([char-set::exponent-start (char-set #\E #\e)])
      (lambda (s i)
        (let ([j1 (match-char-set char-set::exponent-start s i)])
          (if j1 (match<digit-sequence> s j1) #f)))))

  (define (match<exponent?> s i)
    (let ([j1 (match<exponent> s i)])
      (if j1 j1 i)))

  (define match<integer-constant> match<digit-sequence>)

  (define (match<fractional-constant> s i)
    (let ([j1 (match<digit-sequence> s i)])
      (if j1
          (let ([j2 (match<period> s j1)])
            (if j2 (match<digit-sequence?> s j2) #f))
          (let ([j2 (match<period> s i)])
            (if j2 (match<digit-sequence> s j2) #f)))))

  (define (match<floating-point-constant> s i)
    (let ([j1 (match<fractional-constant> s i)])
      (if j1
          (match<exponent?> s j1)
          (let ([j2 (match<digit-sequence> s i)])
            (if j2 (match<exponent> s j2) #f)))))

  (define (match<nonnegative-number> s i)
    ;; Accept negative number, but drop the sign, as suggested in SVG
    ;; 1.1, appendix F.6.2.
    (let-values ([(j v) (match<number> s i)])
      (if j
          (values j (abs v))
          (values #f #f))))

  (define (match<number> s i)
    (let ([i1 (match<sign?> s i)])
      (let ([j1 (match<floating-point-constant> s i1)])
        (if j1
            (values j1 (string->number (substring s i j1)))
            (let ([j2 (match<integer-constant> s i1)])
              (if j2
                  (values j2 (string->number (substring s i j2)))
                  (values #f #f)))))))

  (define match<coordinate> match<number>)

  (define (match<coordinate-pair> s i)
    (let-values ([(j1 v1) (match<coordinate> s i)])
      (if j1
          (let ([j2 (match<comma-wsp?> s j1)])
            (let-values ([(j3 v3) (match<coordinate> s j2)])
              (if j3
                  (values j3 (list v1 v3))
                  (values #f #f))))
          (values #f #f))))

  (define (match<coordinate-double-pair> s i)
    (let-values ([(j1 v1) (match<coordinate-pair> s i)])
      (if j1
          (let ([j2 (match<comma-wsp?> s j1)])
            (let-values ([(j3 v3) (match<coordinate-pair> s j2)])
              (if j3
                  (values j3 (list v1 v3))
                  (values #f #f))))
          (values #f #f))))

  (define (match<coordinate-triple-pair> s i)
    (let-values ([(j1 v1) (match<coordinate-double-pair> s i)])
      (if j1
          (let ([j2 (match<comma-wsp?> s j1)])
            (let-values ([(j3 v3) (match<coordinate-pair> s j2)])
              (if j3
                  (values j3 (append v1 (list v3)))
                  (values #f #f))))
          (values #f #f))))

  (define (match<elliptical-arc-argument> s i)
    (let-values ([(j1 v1) (match<nonnegative-number> s i)])
      (if j1
          (let ([j2 (match<comma-wsp?> s j1)])
            (let-values ([(j3 v3) (match<nonnegative-number> s j2)])
              (if j3
                  (let ([j4 (match<comma-wsp?> s j3)])
                    (let-values ([(j5 v5) (match<number> s j4)])
                      (if j5
                          (let ([j6 (match<comma-wsp?> s j5)])
                            (let-values ([(j7 v7) (match<flag> s j6)])
                              (if j7
                                  (let ([j8 (match<comma-wsp?> s j7)])
                                    (let-values ([(j9 v9) (match<flag> s j8)])
                                      (if j9
                                          (let ([j10 (match<comma-wsp?> s j9)])
                                            (let-values ([(j11 v11) (match<coordinate-pair> s j10)])
                                              (if j11
                                                  (values j11 (list v1 v3 (mod v5 360) v7 v9 v11))
                                                  (values #f #f))))
                                          (values #f #f))))
                                  (values #f #f))))
                          (values #f #f))))
                  (values #f #f))))
          (values #f #f))))

  (define (match-continuation match-component s i prior)
    (let ([j1 (match<comma-wsp?> s i)])
      (let-values ([(j2 v2) (match-component s j1)])
        (if j2
            (match-continuation match-component s j2 (cons v2 prior))
            (values i (reverse prior))))))

  (define (match-sequence match-component s i)
    (let-values ([(j1 v1) (match-component s i)])
      (if j1
          (match-continuation match-component s j1 (list v1))
          (values #f #f))))

  (define (match<coordinate-sequence> s i)
    (match-sequence match<coordinate> s i))

  (define (match<coordinate-pair-sequence> s i)
    (match-sequence match<coordinate-pair> s i))

  (define (match<coordinate-double-pair-sequence> s i)
    (match-sequence match<coordinate-double-pair> s i))

  (define (match<coordinate-triple-pair-sequence> s i)
    (match-sequence match<coordinate-triple-pair> s i))

  (define (expand-command operation arguments)
    (let ([continuation-op (match operation
                             [#\M #\L]
                             [#\m #\l]
                             [other other])])
      (reverse
       (fold-left (lambda (prior arg) (acons continuation-op arg prior))
                  `((,operation . ,(car arguments)))
                  (cdr arguments)))))

  (define (command-matcher cs match-arguments)
    (lambda (s i)
      (let ([j1 (match-char-set cs s i)])
        (if j1
            (let ([j2 (match<wsp*> s j1)])
              (let-values ([(j3 v3) (match-arguments s j2)])
                (if j3
                    (values j3 (expand-command (string-ref s i) v3))
                    (values #f #f))))
            (values #f #f)))))

  (define match<moveto-argument-sequence> match<coordinate-pair-sequence>)
  (define match<moveto>
    (command-matcher (char-set #\M #\m) match<moveto-argument-sequence>))

  (define match<closepath>
    (let ([char-set::closepath (char-set #\Z #\z)])
      (lambda (s i)
        (let ([j (match-char-set char-set::closepath s i)])
          (if j
              (values j (list (list (string-ref s i))))
              (values #f #f))))))

  (define match<lineto-argument-sequence> match<coordinate-pair-sequence>)
  (define match<lineto>
    (command-matcher (char-set #\L #\l) match<lineto-argument-sequence>))

  (define match<horizontal-lineto-argument-sequence> match<coordinate-sequence>)
  (define match<horizontal-lineto>
    (command-matcher (char-set #\H #\h) match<horizontal-lineto-argument-sequence>))

  (define match<vertical-lineto-argument-sequence> match<coordinate-sequence>)
  (define match<vertical-lineto>
    (command-matcher (char-set #\V #\v) match<vertical-lineto-argument-sequence>))

  (define match<curveto-argument-sequence> match<coordinate-triple-pair-sequence>)
  (define match<curveto>
    (command-matcher (char-set #\C #\c) match<curveto-argument-sequence>))

  (define match<smooth-curveto-argument-sequence> match<coordinate-double-pair-sequence>)
  (define match<smooth-curveto>
    (command-matcher (char-set #\S #\s) match<smooth-curveto-argument-sequence>))

  (define match<quadratic-bezier-curveto-argument-sequence> match<coordinate-double-pair-sequence>)
  (define match<quadratic-bezier-curveto>
    (command-matcher (char-set #\Q #\q) match<quadratic-bezier-curveto-argument-sequence>))

  (define match<smooth-quadratic-bezier-curveto-argument-sequence> match<coordinate-pair-sequence>)
  (define match<smooth-quadratic-bezier-curveto>
    (command-matcher (char-set #\T #\t) match<smooth-quadratic-bezier-curveto-argument-sequence>))

  (define (match<elliptical-arc-argument-sequence> s i)
    (match-sequence match<elliptical-arc-argument> s i))
  (define match<elliptical-arc>
    (command-matcher (char-set #\A #\a) match<elliptical-arc-argument-sequence>))

  (define match<drawto-command>
    (let ([drawto-matchers
           (list match<closepath>
                 match<lineto>
                 match<horizontal-lineto>
                 match<vertical-lineto>
                 match<curveto>
                 match<smooth-curveto>
                 match<quadratic-bezier-curveto>
                 match<smooth-quadratic-bezier-curveto>
                 match<elliptical-arc>)])
      (lambda (s i)
        (letrec ([match-cmd 
                  (match-lambda [() (values #f #f)]
                                [(matcher . more-matchers)
                                 (let-values ([(j v) (matcher s i)])
                                   (if j
                                       (values j v)
                                       (match-cmd more-matchers)))])])
          (match-cmd drawto-matchers)))))

  (define (match-drawto-commands-continuation s i prior)
    (let ([j1 (match<wsp*> s i)])
      (let-values ([(j2 v2) (match<drawto-command> s j1)])
        (if j2
            (match-drawto-commands-continuation s j2
                                                (append prior v2))
            (values i prior)))))

  (define (match<drawto-commands> s i)
    (let-values ([(j1 v1) (match<drawto-command> s i)])
      (if j1
          (match-drawto-commands-continuation s j1 v1)
          (values #f #f))))

  (define (match<moveto-drawto-command-group> s i)
    (let-values ([(j1 v1) (match<moveto> s i)])
      (if j1
          (let ([j2 (match<wsp*> s j1)])
            (let-values ([(j3 v3) (match<drawto-commands> s j2)])
              (if j3
                  (values j3 (append v1 v3))
                  (values j2 v1))))
          (values #f #f))))

  (define (match-moveto-drawto-command-groups-continuation s i prior)
    (let ([j1 (match<wsp*> s i)])
      (let-values ([(j2 v2) (match<moveto-drawto-command-group> s j1)])
        (if j2
            (match-moveto-drawto-command-groups-continuation s j2
                                                             (append prior v2))
            (values i prior)))))

  (define (match<moveto-drawto-command-groups> s i)
    (let-values ([(j1 v1) (match<moveto-drawto-command-group> s i)])
      (if j1
          (match-moveto-drawto-command-groups-continuation s j1 v1)
          (values #f #f))))

  (define (match<svg-path> s i)
    (let ([j1 (match<wsp*> s i)])
      (let-values ([(j2 v2) (match<moveto-drawto-command-groups> s j1)])
        (if j2
            (let ([j3 (match<wsp*> s j2)])
              (values j3 v2))
            (values j1 '())))))

  (define (parse-path-data s)
    (let-values ([(j v) (match<svg-path> s 0)])
      (if (and j (= j (string-length s)))
          v
          v ;; FIXME: Log an error message or raise an exception if this happens.
          )))

  (define/kwargs (svg:parse-path-data path-data-string [stage 'absolute-coords])
    (match stage
      ['parse-only (parse-path-data path-data-string)]
      ['subpath-lists (svg:path-data-subpaths
                       (svg:parse-path-data path-data-string 'parse-only))]
      ['subpath-vectors (svg:subpaths->vectors
                         (svg:parse-path-data path-data-string 'subpath-lists))]
      ['absolute-coords (svg:make-subpath-vectors-absolute!
                         (svg:parse-path-data path-data-string 'subpath-vectors))]
      ['quadratic (assertion-violation 'svg:parse-path-data
                                       "not yet implemented"
                                       stage)]
      ['cubic (svg:expand-elliptic-arcs!
               (svg:subpaths-quadratic->cubic!
                (svg:parse-path-data path-data-string 'absolute-coords))
               3)]
      [_ (assertion-violation
          'svg:parse-path-data
          "expected 'parse-only, 'subpath-lists, 'subpath-vectors, 'absolute-coords, 'quadratic, or 'cubic"
          stage)] ))

  ;;-------------------------------------------------------------------------

  (define (split-after-subpath commands)
    (letrec ([up-to-closepath
              (lambda (prior cmds)
                (match cmds
                  [()
                   (values (reverse prior) '())]
                  [(((or #\Z #\z)) . t)
                   (values (reverse (cons '(#\Z) prior)) t)]
                  [(((or #\M #\m) . _) . t)
                   (values (reverse prior) cmds)]
                  [(h . t)
                   (up-to-closepath (cons h prior) t)]))])
      (match commands
        [(((or #\M #\m) . _) . t)
         (up-to-closepath (list (car commands)) (cdr commands))]
        [_ (up-to-closepath '((#\m 0 0)) commands)] )))

  (define (split-into-subpaths commands)
    (letrec ([split
              (lambda (prior cmds)
                (if (null? cmds)
                    (reverse prior)
                    (let-values ([(subpath more-subpaths)
                                  (split-after-subpath cmds)])
                      (split (cons subpath prior) more-subpaths))))])
      (split '() commands)))

  (define (svg:path-data-subpaths commands)
    (split-into-subpaths commands))

  ;;-------------------------------------------------------------------------

  (define (svg:subpaths->vectors subpaths)
    (if (vector? subpaths)
        (vector-map (lambda (subpath) (if (vector? subpath) subpath (list->vector subpath)))
                    subpaths)
        (list->vector
         (map (lambda (subpath) (if (vector? subpath) subpath (list->vector subpath)))
              subpaths))))

  (define (svg:subpaths->lists subpaths)
    (if (vector? subpaths)
        (vector->list
         (vector-map (lambda (subpath) (if (vector? subpath) (vector->list subpath) subpath))
                     subpaths))
        (map (lambda (subpath) (if (vector? subpath) (vector->list subpath) subpath))
             subpaths)))

  ;;-------------------------------------------------------------------------

  (define (final-point vec i)
    (match (vector-ref vec i)
      [((or #\M #\L #\T) . coords) coords]
      [((or #\Q #\S) . (_ coords)) coords]
      [((or #\C) . (_ _ coords))   coords]
      [(#\A . (_ _ _ _ _ coords))  coords]
      [(#\Z) (final-point vec 0)]))

  (define (make-command-absolute vec i current-point)
    (let ([x0 (car current-point)]
          [y0 (cadr current-point)])
      (match (vector-ref vec i)
        [(#\m x y) `(#\M ,(+ x0 x) ,(+ y0 y))]
        [(#\l x y) `(#\L ,(+ x0 x) ,(+ y0 y))]
        [(#\t x y) `(#\T ,(+ x0 x) ,(+ y0 y))]
        [(#\q (x1 y1) (x2 y2))
         `(#\Q (,(+ x0 x1) ,(+ y0 y1)) (,(+ x0 x2) ,(+ y0 y2)))]
        [(#\s (x1 y1) (x2 y2))
         `(#\S (,(+ x0 x1) ,(+ y0 y1)) (,(+ x0 x2) ,(+ y0 y2)))]
        [(#\c (x1 y1) (x2 y2) (x3 y3))
         `(#\C (,(+ x0 x1) ,(+ y0 y1)) (,(+ x0 x2) ,(+ y0 y2)) (,(+ x0 x3) ,(+ y0 y3)))]
        [(#\a rx ry rotation fA fS (x y))
         `(#\A ,rx ,ry ,rotation ,fA ,fS (,(+ x0 x) ,(+ y0 y)))]
        [(#\h . x) `(#\L ,(+ x0 x) ,y0)]
        [(#\v . y) `(#\L ,x0 ,(+ y0 y))]
        [(#\H . x) `(#\L ,x ,y0)]
        [(#\V . y) `(#\L ,x0 ,y)]
        [other other])))

  (define (reflect-control-point! vec i)
    ;; Convert #\S commands to #\C and #\T commands to #\Q.
    (match (vector-ref vec i)
      [(#\S (x2 y2) (x3 y3))
       (vector-set! vec i
                    (match (vector-ref vec (- i 1))
                      [(#\C (_ _) (x2^ y2^) (x3^ y3^))
                       (let ([x1 (- (+ x3^ x3^) x2^)]
                             [y1 (- (+ y3^ y3^) y2^)])
                         `(#\C (,x1 ,y1) (,x2 ,y2) (,x3 ,y3)))]
                      [_ (let* ([curpt (final-point vec (- i 1))]
                                [x0 (car curpt)]
                                [y0 (cadr curpt)])
                           `(#\C (,x0 ,y0) (,x2 ,y2) (,x3 ,y3)))] ))]
      [(#\T x2 y2)
       (vector-set! vec i
                    (match (vector-ref vec (- i 1))
                      [(#\Q (x1^ y1^) (x2^ y2^))
                       (let ([x1 (- (+ x2^ x2^) x1^)]
                             [y1 (- (+ y2^ y2^) y1^)])
                         `(#\Q (,x1 ,y1) (,x2 ,y2)))]
                      [_ (let* ([curpt (final-point vec (- i 1))]
                                [x0 (car curpt)]
                                [y0 (cadr curpt)])
                           `(#\Q (,x0 ,y0) (,x2 ,y2)))] ))]
      [_ *unspecified*]))

  (define (adjust-elliptic-arc-radii! vec i)
    ;; See SVG 1.1 specification, appendix F.6.6.
    (match (vector-ref vec i)
      [(\#A rx ry rotation fA fS point)
       (if (or (zero? rx) (zero? ry))

           ;; If either axis is zero, just draw a straight line.
           (vector-set! vec i `(#\L . ,point))

           ;; If the axes are too small, increase their size just
           ;; enough.
           (let* ([curpt (final-point vec (- i 1))]
                  [x₁ (car curpt)]
                  [y₁ (cadr curpt)]
                  [x₂ (car point)]
                  [y₂ (cadr point)]
                  [φ (/ (* pi rotation) 180)]
                  [cosφ (cos φ)]
                  [sinφ (sin φ)]
                  [x₁^ (transformed-x₁ x₁ y₁ x₂ y₂ cosφ sinφ)]
                  [y₁^ (transformed-y₁ x₁ y₁ x₂ y₂ cosφ sinφ)]
                  [Λ (+ (/ (* x₁^ x₁^) (* rx rx)) (/ (* y₁^ y₁^) (* ry ry)))])
             (when (< 1 Λ)
               (let ([sqrtΛ (sqrt Λ)])
                 (vector-set! vec i `[#\A ,(* sqrtΛ rx) ,(* sqrtΛ ry)
                                      ,rotation ,fA ,fS ,point] ))) ))]

      [_ *unspecified*]))

  (define (make-subpath-vector-absolute! vec current-point)
    (vector-set! vec 0 (make-command-absolute vec 0 current-point))
    (do ([i 1 (+ i 1)]) ([= i (vector-length vec)])
      (let ([point (final-point vec (- i 1))])
        (vector-set! vec i (make-command-absolute vec i point))
        (reflect-control-point! vec i)
        (adjust-elliptic-arc-radii! vec i)))
    (final-point vec (- (vector-length vec) 1)))

  (define (svg:make-subpath-vectors-absolute! subpath-vectors)
    (let ([current-point '(0 0)])
      (do ([i 0 (+ i 1)]) ([= i (vector-length subpath-vectors)])
        (let ([new-point (make-subpath-vector-absolute!
                          (vector-ref subpath-vectors i) current-point)])
          (set! current-point new-point)))
      (values subpath-vectors current-point)))

  ;;-------------------------------------------------------------------------

  (define (quadratic-curveto->cubic! vec i)
    (match (vector-ref vec i)
      [(#\Q (x1 y1) (x2 y2))
       (let* ([curpt (final-point vec (- i 1))]
              [x0 (car curpt)]
              [y0 (cadr curpt)]
              [quad-xvec (list->vector `(,x0 ,x1 ,x2))]
              [quad-yvec (list->vector `(,y0 ,y1 ,y2))]
              [cubic-xvec (poly:elev-scm-bern 3 quad-xvec)]
              [cubic-yvec (poly:elev-scm-bern 3 quad-yvec)])
         (vector-set! vec i
                      (match cubic-xvec
                        [#(_ cx1 cx2 _)
                         (match cubic-yvec
                           [#(_ cy1 cy2 _)
                            `(#\C (,cx1 ,cy1) (,cx2 ,cy2) (,x2 ,y2))])])))]
      [_ *unspecified*] ))

  (define (vector-quadratic->cubic! vec)
    (do ([i 1 (+ i 1)]) ([= i (vector-length vec)])
      (quadratic-curveto->cubic! vec i)))

  (define (svg:subpaths-quadratic->cubic! subpath-vectors)
    (do ([i 0 (+ i 1)]) ([= i (vector-length subpath-vectors)])
      (vector-quadratic->cubic! (vector-ref subpath-vectors i)))
    subpath-vectors)

  ;;-------------------------------------------------------------------------

  (define (transformed-x₁ x₁ y₁ x₂ y₂ cosφ sinφ)
    (/ (+ (* cosφ (- x₁ x₂)) (* sinφ (- y₁ y₂))) 2))

  (define (transformed-y₁ x₁ y₁ x₂ y₂ cosφ sinφ)
    (/ (- (* cosφ (- y₁ y₂)) (* sinφ (- x₁ x₂))) 2))

  (define (transformed-start-point x₁ y₁ x₂ y₂ cosφ sinφ)
    (let ([x₁^ (transformed-x₁ x₁ y₁ x₂ y₂ cosφ sinφ)]
          [y₁^ (transformed-y₁ x₁ y₁ x₂ y₂ cosφ sinφ)])
      (values x₁^ y₁^)))

  (define (center-of-transformed-ellipse x₁^ y₁^ fA fS rx ry cosφ sinφ)
    (let* ([rx² (* rx rx)]
           [ry² (* ry ry)]
           [rx²y₁^²+ry²x₁^² (+ (* rx² y₁^ y₁^) (* ry² x₁^ x₁^))]
           [sign (if (eq? fA fS) -1 1)]

           ;; The @code{(max 0 ...)} below is to ensure that roundoff
           ;; does not result in a complex radical. (I do not know if
           ;; this actually happens, but a comment in the old
           ;; FontForge code suggests it might. The treatment there
           ;; was to snap values near zero to zero exactly, rather
           ;; than what we have done here.)
           [factor
            (let ([numer (max 0 (- (* rx² ry²) rx²y₁^²+ry²x₁^²))])
              (* sign (/ numer rx²y₁^²+ry²x₁^²)))]
           [cx^ (/ (* factor rx y₁^) ry)]
           [cy^ (/ (* factor ry x₁^) rx)])
      (values cx^ cy^)))

  (define (center-of-ellipse cx^ cy^ x₁ y₁ x₂ y₂ cosφ sinφ)
    (values (+ (- (* cosφ cx^) (* sinφ cy^)) (/ (+ x₁ x₂) 2))
            (+ (+ (* cosφ cy^) (* sinφ cx^)) (/ (+ y₁ y₂) 2))))

  (define (endpoint-angles x₁ y₁ x₂ y₂ cx cy fS)
    (let* ([z₁ (make-rectangular (- x₁ cx) (- y₁ cy))]
           [z₂ (make-rectangular (- x₂ cx) (- y₂ cy))]
           [θ₁ (angle z₁)]
           [θ₂ₜ (angle z₂)]
           [θ₂ (if fS
                   (if (< θ₁ θ₂ₜ) (- θ₂ₜ pi pi) θ₂ₜ)
                   (if (< θ₂ₜ θ₁) (+ θ₂ₜ pi pi) θ₂ₜ))])
      (values θ₁ θ₂)))

  (define svg:elliptic-arc-quadratic-spline-count-max (make-fluid 128))
  (define svg:elliptic-arc-cubic-spline-count-max (make-fluid 128))

  (define svg:elliptic-arc-quadratic-error-threshold (make-fluid 1/100))
  (define svg:elliptic-arc-cubic-error-threshold (make-fluid 1/100))

  (define/kwargs (expand-arc x₁ y₁ x₂ y₂ fA fS rx ry rotation degree
                             [spline-count-max #f]
                             [error-threshold #f])
    (assert (positive? rx))
    (assert (positive? ry))
    (assert (<= 2 degree 3))
    (let ([spline-count-max
           (if spline-count-max spline-count-max
               (case degree
                 [(2) (fluid-ref svg:elliptic-arc-quadratic-spline-count-max)]
                 [(3) (fluid-ref svg:elliptic-arc-cubic-spline-count-max)]))]
          [error-threshold
           (if error-threshold error-threshold
               (case degree
                 [(2) (fluid-ref svg:elliptic-arc-quadratic-error-threshold)]
                 [(3) (fluid-ref svg:elliptic-arc-cubic-error-threshold)]))])
      (let*-values
          ;; Modify the problem so instead of the original @var{rx} and
          ;; @var{ry} we are working with the semi-major and semi-minor
          ;; axes, and @var{φ}, the (possibly negative)
          ;; counterclockwise rotation with respect to the semi-major
          ;; axis.
          ([(semimajor semiminor φ)
            (if (<= ry rx)
                (values rx ry (/ (* pi (mod rotation 360)) -180))
                (values ry rx (/ (* pi (mod (+ 90 rotation) 360)) -180)))]
           [(cosφ sinφ) (values (cos φ) (sin φ))]
           [(xsplines ysplines)
            (if (and (RealNear? x₁ x₂) (RealNear? y₁ y₂))
                (let* ([sign (if (eq? fA fS) -1 1)]
                       [cx (+ x₁ (* sign (+ (* rx cosφ) (* ry sinφ))))]
                       [cy (+ y₁ (* sign (- (* ry cosφ) (* rx sinφ))))])
                  (elliptic-arc-piecewise-bezier degree spline-count-max
                                                 error-threshold
                                                 semimajor semiminor
                                                 cx cy φ 0 (* 2 pi)))
                (let*-values
                    ([(x₁^ y₁^) (transformed-start-point x₁ y₁ x₂ y₂ cosφ sinφ)]
                     [(cx^ cy^) (center-of-transformed-ellipse
                                 x₁^ y₁^ fA fS semimajor semiminor cosφ sinφ)]
                     [(cx cy) (center-of-ellipse cx^ cy^ x₁ y₁ x₂ y₂ cosφ sinφ)]
                     [(λ₁ λ₂) (endpoint-angles x₁ y₁ x₂ y₂ cx cy fS)])
                  (elliptic-arc-piecewise-bezier degree spline-count-max
                                                 error-threshold
                                                 semimajor semiminor
                                                 cx cy φ λ₁ λ₂)))])
        ;; Store the exact endpoints.
        (matrix-1set! xsplines 1 1 x₁)
        (matrix-1set! ysplines 1 1 y₁)
        (matrix-1set! xsplines
                      (matrix-row-count xsplines)
                      (matrix-column-count xsplines)
                      x₂)
        (matrix-1set! ysplines
                      (matrix-row-count ysplines)
                      (matrix-column-count ysplines)
                      y₂)

        (values xsplines ysplines))))

  #|
  (define/kwargs (expand-arc x₁ y₁ x₂ y₂ fA fS rx ry rotation degree
                             [spline-count-max #f]
                             [error-threshold #f])
    (assert (positive? rx))
    (assert (positive? ry))
    (assert (<= 2 degree 3))
    (let ([spline-count-max
           (if spline-count-max spline-count-max
               (case degree
                 [(2) (fluid-ref svg:elliptic-arc-quadratic-spline-count-max)]
                 [(3) (fluid-ref svg:elliptic-arc-cubic-spline-count-max)]))]
          [error-threshold
           (if error-threshold error-threshold
               (case degree
                 [(2) (fluid-ref svg:elliptic-arc-quadratic-error-threshold)]
                 [(3) (fluid-ref svg:elliptic-arc-cubic-error-threshold)]))])
      (let*-values
          ;; Modify the problem so instead of the original @var{rx} and
          ;; @var{ry} we are working with the semi-major and semi-minor
          ;; axes, and @var{φ}, the (possibly negative)
          ;; counterclockwise rotation with respect to the semi-major
          ;; axis.
          ([(semimajor semiminor φ)
            (if (<= ry rx)
                (values rx ry (/ (* pi (mod rotation 360)) -180))
                (values ry rx (/ (* pi (mod (+ 90 rotation) 360)) -180)))]
           [(cosφ sinφ) (values (cos φ) (sin φ))]
           [(x₁^ y₁^) (transformed-start-point x₁ y₁ x₂ y₂ cosφ sinφ)]
           [(cx^ cy^) (center-of-transformed-ellipse
                       x₁^ y₁^ fA fS semimajor semiminor cosφ sinφ)]
           [(cx cy) (center-of-ellipse cx^ cy^ x₁ y₁ x₂ y₂ cosφ sinφ)]
           [(λ₁ λ₂) (endpoint-angles x₁ y₁ x₂ y₂ cx cy fS)]
           [(xsplines ysplines)
            (elliptic-arc-piecewise-bezier degree spline-count-max error-threshold
                                           semimajor semiminor cx cy φ λ₁ λ₂)])

        ;; Store the exact endpoints.
        (matrix-1set! xsplines 1 1 x₁)
        (matrix-1set! ysplines 1 1 y₁)
        (matrix-1set! xsplines
                      (matrix-row-count xsplines)
                      (matrix-column-count xsplines)
                      x₂)
        (matrix-1set! ysplines
                      (matrix-row-count ysplines)
                      (matrix-column-count ysplines)
                      y₂)

        (values xsplines ysplines))))
  |#

  (define (spline->command xspline yspline)
    (case (row-matrix-size xspline)
      [(3) `(#\Q
             (,(matrix-1ref xspline 1 2) ,(matrix-1ref yspline 1 2))
             (,(matrix-1ref xspline 1 3) ,(matrix-1ref yspline 1 3)))]
      [(4) `(#\C
             (,(matrix-1ref xspline 1 2) ,(matrix-1ref yspline 1 2))
             (,(matrix-1ref xspline 1 3) ,(matrix-1ref yspline 1 3))
             (,(matrix-1ref xspline 1 4) ,(matrix-1ref yspline 1 4)))]))

  (define (splines->commands xsplines ysplines)
    (assert (equal? (matrix-dimensions xsplines) (matrix-dimensions ysplines)))
    (assert (<= 3 (matrix-column-count xsplines) 4))
    (map
     (lambda (i)
       (spline->command (matrix-0row xsplines i) (matrix-0row ysplines i)))
     (iota (matrix-row-count xsplines))))

  (define (expand-elliptic-arc degree vec i)
    (match (vector-ref vec i)
      [(#\A rx ry rotation fA fS (x y))
       (let* ([curpt (final-point vec (- i 1))]
              [x0 (car curpt)]
              [y0 (cadr curpt)])
         (let-values ([(xsplines ysplines)
                       (expand-arc x0 y0 x y fA fS rx ry rotation degree)])
           (splines->commands xsplines ysplines)))]
      [other (list other)]))

  (define (expand-elliptic-arcs degree vec)
    (reverse
     (fold-left
      (lambda (prior i)
        (let ([expansion (expand-elliptic-arc degree vec i)])
          (append (reverse expansion) prior)))
      (list (vector-ref vec 0))
      (iota (- (vector-length vec) 1) 1))))

  (define/kwargs (svg:expand-elliptic-arcs! subpath-vectors degree)
    (do ([i 0 (+ i 1)]) ([= i (vector-length subpath-vectors)])
      (vector-set! subpath-vectors i
                   (list->vector
                    (expand-elliptic-arcs degree
                                          (vector-ref subpath-vectors i)))))
    subpath-vectors)

  ;;-------------------------------------------------------------------------

  ) ;; end of library.
