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

(library (sortsmill svg)

  (export )

  (import (rnrs)
          (except (guile) error))

;;;;;; FIXME: Put this stuff in a special ‘submodule’ for path data
;;;;;; attributes.

  (define char-set::comma (char-set #\,))
  (define char-set::period (char-set #\.))
  (define char-set::wsp (char-set #\space #\tab #\return #\newline))
  (define char-set::flag (char-set #\0 #\1))
  (define char-set::digit (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (define char-set::sign (char-set #\+ #\-))
  (define char-set::exponent-start (char-set #\E #\e))

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

  (define (match<comma> s i) (match-char-set char-set::comma s i))
  (define (match<comma?> s i) (match-char-set<?> char-set::comma s i))

  (define (match<period> s i) (match-char-set char-set::period s i))

  (define (match<sign?> s i) (match-char-set<?> char-set::sign s i))

  (define (match<wsp*> s i) (match-char-set<*> char-set::wsp s i))
  (define (match<wsp+> s i) (match-char-set<+> char-set::wsp s i))

  (define (match<digit*> s i) (match-char-set<*> char-set::digit s i))
  (define (match<digit+> s i) (match-char-set<+> char-set::digit s i))

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

  (define (match<exponent> s i)
    (let ([j1 (match-char-set char-set::exponent-start s i)])
      (if j1 (match<digit-sequence> s j1) #f)))

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
          (values j1 (substring s i j1))
          (let ([j2 (match<integer-constant> s i)])
            (if j2
                (values j2 (substring s i j2))
                (values #f #f))))))

  (define (match<number> s i)
    (let ([i1 (match<sign?> s i)])
      (let ([j1 (match<floating-point-constant> s i1)])
        (if j1
            (values j1 (substring s i j1))
            (let ([j2 (match<integer-constant> s i1)])
              (if j2
                  (values j2 (substring s i j2))
                  (values #f #f)))))))

  (define match<coordinate> match<number>)

  (define (match<coordinate-pair> s i)
    (let ([j1 (match<coordinate> s i)])
      (if j1
          (let* ([j2 (match<comma-wsp?> s j1)]
                 [j3 (match<coordinate> s j2)])
            (if j3
                (values j3 (list (substring s i j1)
                                 (substring s j2 j3)))
                (values #f #f)))
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

;;;  (let-values ([(i v) (match-sequence match<coordinate-pair> "+12354.2e222-32.,30 40,40 , 50" 0)])
;;;    (write (list i v)))

  ) ;; end of library.
