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

(library (sortsmill svg path-data)

  (export
   ;; (svg:parse-path-data string) → list
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
   svg:parse-path-data
   )

  (import (rnrs)
          (except (guile) error)
          (only (srfi :26) cut)
          (ice-9 match))

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

  (define match<flag>
    (let ([char-set::flag (char-set #\0 #\1)])
      (lambda (s i)
        (let ([j (match-char-set char-set::flag s i)])
          (if j
              (values j (char=? #\1 (string-ref s i)))
              (values #f #f))))))

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
    (let ([j1 (match<floating-point-constant> s i)])
      (if j1
          (values j1 (string->number (substring s i j1)))
          (let ([j2 (match<integer-constant> s i)])
            (if j2
                (values j2 (string->number (substring s i j2)))
                (values #f #f))))))

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
                                                  (values j11 (list v1 v3 v5 v7 v9 v11))
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

  (define (svg:parse-path-data s)
    (let-values ([(j v) (match<svg-path> s 0)])
      (if (and j (= j (string-length s)))
          v
          #f ;; FIXME: Log an error message or raise an exception if this happens.
          )))

  ) ;; end of library.
