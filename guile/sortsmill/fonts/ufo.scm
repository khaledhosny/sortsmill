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

(library (sortsmill fonts ufo)

  (export ufo:read-metainfo
          ufo:read-fontinfo)

  (import (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (only (srfi :26) cut)
          (sxml simple)
          (sxml match)
          (ice-9 match))

  (define (ufo:read-metainfo ufo)
    (parse-dict-xml ufo "metainfo.plist"))

  (define (ufo:read-fontinfo ufo)
    (parse-dict-xml ufo "fontinfo.plist"))

  (define (parse-dict-xml ufo . components)
    (match (apply parse-ufo-xml ufo components)
      ['file-not-found 'file-not-found]
      [lst (isolated-dict lst)] ))

  (define (isolated-dict lst)
    (let ([obj (isolated-element lst)])
      (if (ufo-dict? obj)
          obj
          (error 'isolated-dict (_ "expected a <dict>") obj))))

  (define (isolated-element lst)
    (match lst
      [(element) element]
      [_ (error 'one-toplevel-element
                (_ "expected one and only one element")
                lst)] ))

  (define (ufo-dict? obj)
    (and (list? obj)
         (for-all (lambda (p) (and (pair? p) (symbol? (car p)))) obj)))

  (define (parse-ufo-xml ufo . components)
    (let ([sxml (apply read-ufo-xml ufo components)])
      (if sxml (match-ufo-element sxml) 'file-not-found)))

  (define (read-ufo-xml ufo . components)
    (let ([file-name (apply ufo-file-name ufo components)])
      (if (file-exists? file-name)
          (call-with-input-file file-name
            (cut xml->sxml <> #:trim-whitespace? #t))
          #f)))

  (define (match-ufo-element expr)
    (sxml-match expr
      [(*TOP* (*PI* ,pi ...) ,[e]) e]
      [(*TOP* ,[e]) e]
      [(plist ,[e] ...) (list e ...)]
      [(string ,s) s]
      [(string) ""]
      [(integer ,n) (string->number n)]
      [(real ,x) (string->number x)]
      [(true) #t]
      [(false) #f]
      [(array ,[e] ...) (list e ...)]
      [(dict . ,pairs) (match-dict-pairs pairs)]
      [,other (error 'match-ufo-element
                     (_ "unexpected SXML")
                     other)] ))

  (define* (match-dict-pairs pairs #:optional [prior '()])
    (sxml-match pairs
      [(list) (reverse prior)]
      [(list (key ,k) ,e . ,more-pairs)
       (match-dict-pairs
        more-pairs
        (acons (string->symbol k) (match-ufo-element e) prior))]
      [,other (error 'match-dict-pairs
                     (_ "unexpected SXML")
                     other)] ))

  (define (ufo-file-name ufo . components)
    (string-join (cons ufo components) "/"))

  ) ;; end of library.
