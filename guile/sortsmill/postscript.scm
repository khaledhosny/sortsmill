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

(library (sortsmill postscript)

;;; FIXME: This module is going to go away in favor of a Core Guile
;;; equivalent.

  (export
   ;; (scm->postscript various-types) → string
   scm->postscript

   ;; These currently are reëxported from Core Guile.
   ;;
   ;; (postscript-number? string) → boolean
   ;; (postscript->number string) → real
   postscript-number?
   postscript->number

   ;; (postscript-boolean? string) → boolean
   ;; (postscript->boolean string) → boolean
   postscript-boolean?
   postscript->boolean

   ;; (postscript-number-list? string) → boolean
   ;; (postscript->number-list string) → number-list
   postscript-number-list?
   postscript->number-list
   )

  (import (sortsmill core)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (ice-9 format)
          (ice-9 match))

  (define (scm->postscript value)
    (cond
     [(eq? value #f) "false"]
     [(eq? value #t) "true"]
     [(string? value) value]
     [(integer? value) (format #f "~d" value)]
     [(real? value) (format #f "~f" value)]
     [(list? value)
      (let ([strings (map scm->postscript value)])
        (string-append "[" (string-join strings " ") "]"))]
     [else (assertion-violation 'scm->postscript
                                (_ "unexpected argument value")
                                value)] ))

  (define (postscript-boolean? s)
    (if (string? s)
        (with-input-from-string s
          (lambda ()
            (match (read)
              [(or 'true 'false) #t]
              [_ #f])))
        #f))

  (define (postscript->boolean s)
    (with-input-from-string s
      (lambda ()
        (match (read)
          ['true #t]
          ['false #f]
          [_ (assertion-violation 'postscript->boolean
                                  (_ "not a valid PostScript boolean")
                                  s)]))))

  (define (postscript-number-list? s)
    (if (string? s)
        (let ([s^ (string-trim-both s)])
          (cond [(not (string-prefix? "[" s^)) #f]
                [(not (string-suffix? "]" s^)) #f]
                [else (with-input-from-string s^
                        (lambda ()
                          (match (read)
                            [((? real? _) ...) #t]
                            [_ #f])))] ))
        #f))

  (define (postscript->number-list s)
    (let ([erroneous
           (lambda ()
             (assertion-violation 'postscript->number-list
                                  (_ "not a valid PostScript number array")
                                  s))]
          [s^ (string-trim-both s)])
      (cond [(not (string-prefix? "[" s^)) (erroneous)]
            [(not (string-suffix? "]" s^)) (erroneous)]
            [else (with-input-from-string s^
                    (lambda ()
                      (match (read)
                        [(and ((? real? _) ...) lst) lst]
                        [_ (erroneous)] )))] )))

  ) ;; end of library.
