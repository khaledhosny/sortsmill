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

(library (sortsmill strings text-embedding)
  
  (export lines-begin-with)

  (import (rnrs)
          (except (guile) error))

  ;;-------------------------------------------------------------------------
  ;;
  ;; FIXME: This certainly cannot be the fastest implementation of
  ;; @var{lines-begin-with}. A version using regexps might be better,
  ;; for instance. (I am waiting, perhaps, until I have made a
  ;; ‘rexp’-like PCRE interface for Guile, rather than use Guile’s own
  ;; regexp support. See <sortsmill/rexp.h>.)

  (define chars-to-ignore-at-beginning (char-set #\space #\tab))

  (define (skip-chars-at-beginning s i)
    (if (< i (string-length s))
        (let ([c (string-ref s i)])
          (if (char-set-contains? chars-to-ignore-at-beginning c)
              (skip-chars-at-beginning s (+ i 1))
              i))
        i))

  (define (skip-to-end-of-line s i)
    (if (< i (string-length s))
        (let ([c (string-ref s i)])
          (if (char=? c #\newline)
              (+ i 1)
              (skip-to-end-of-line s (+ i 1))))
        i))

  (define (delimit-altered-line s marker i)
    (let ([j (skip-chars-at-beginning s i)])
      (if (< j (string-length s))
          (if (string-prefix? marker (substring s j))
              (let ([n (string-length marker)])
                (values (+ j n) (skip-to-end-of-line s (+ j n))))
              (values i (skip-to-end-of-line s i)))
          (values i j))))

  (define (get-altered-line s marker i)
    (let-values ([(j k) (delimit-altered-line s marker i)])
      (let ([line (substring s j k)])
        (values line k))))

  (define (lines-begin-with marker s)
    (letrec ([get-lines
              (lambda (prior i)
                (if (< i (string-length s))
                    (let-values ([(line j) (get-altered-line s marker i)])
                      (get-lines (string-append prior line) j))
                    prior))])
      (get-lines "" 0)))

  ) ;; end of library.
