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

(library (sortsmill strings ietf-language-tags)

  ;; References.
  ;;
  ;;    http://www.langtag.net/
  ;;
  ;;    http://en.wikipedia.org/wiki/IETF_language_tag
  ;;
  ;;    http://tools.ietf.org/html/rfc5645
  ;;
  ;;    http://tools.ietf.org/html/rfc4647

  (export
   ;; (ietf-language-tag=? tag1 tag2) → boolean
   ;;
   ;; A case-insensitive string comparison. This comparison takes no
   ;; account of language ranges [http://tools.ietf.org/html/rfc4647];
   ;; the match must be exact except for letter case.
   ;;
   ;; FIXME: This is not how comparison should be done.
   ietf-language-tag=?

   ;; (ietf-language-tag=? tag) → tag-in-recommended-format
   ;;
   ;; See http://tools.ietf.org/html/rfc5646, section 2.1.1.
   reformat-ietf-language-tag

   parse-ietf-language-tag
   )

  (import (sortsmill strings rexp)
          (rnrs)
          (except (guile) error)
          (only (srfi :26) cut)
          (srfi :31)                    ; ‘rec’
          (ice-9 i18n)
          (ice-9 format)
          (ice-9 match))

  (define posix-locale (make-locale (list LC_ALL) "C"))

  (define separator "[-_]")             ; Accept #\_ in place of #\-

  (define alpha "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]")
  (define alphanum "[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789]")
  (define singleton "[abcdefghijklmnopqrstuvwyzABCDEFGHIJKLMNOPQRSTUVWYZ0123456789]")
  (define digit "[0123456789]")

  (define tag-end (format #f "(?=~a|\\z)" separator))

  (define extlang-str (format #f "(~a{3}(~a~a{3}(~a~a{3})?)?)~a"
                          alpha separator alpha separator alpha tag-end))

  (define language-str (format #f "(~a{4,8}|~a{2,3}(~a~a)?)~a"
                               alpha alpha separator extlang-str tag-end))

  (define script-str (format #f "~a~a{4}~a" separator alpha tag-end))

  (define region-str (format #f "~a(~a{2}|~a{3})~a"
                             separator alpha digit tag-end))

  (define variant-str (format #f "~a(~a~a{3}|~a~a{4,7})~a"
                              separator digit alphanum alpha alphanum tag-end))

  (define extension-str (format #f "~a(~a(~a~a{2,8})+)~a"
                                separator singleton separator alphanum tag-end))

  (define privateuse-str (format #f "([xX](~a~a{1,8})+)\\z" separator alphanum))

  (define privateuse-subtags-str (format #f "~a~a" separator privateuse-str))

  (define (simple-matcher re-string i)
    (let ([re (rexp:compile-study re-string)])
      (lambda (s)
        (let ([m (rexp:match re s)])
          (if m
              (match (rexp:interval m 0)
                [(_ j) (values (substring s i j)
                               (substring s j))])
              (values #f s))))))

  #|
  (define remove-leading-separator
    (let ([re (rexp:compile-study separator)])
      (lambda (s)
        (let ([m (rexp:match re s)])
           (if m (string-drop s 1) s)))))
  |#

  (define match-language (simple-matcher language-str 0))
  (define match-script (simple-matcher script-str 1))
  (define match-region (simple-matcher region-str 1))
  (define match-variant (simple-matcher variant-str 1))
  (define match-extension (simple-matcher extension-str 1))
  (define match-privateuse-subtags (simple-matcher privateuse-subtags-str 1))

  (define (multiple-matcher match-item)
    (rec (match-items s)
         (let-values ([(item s) (match-item s)])
           (if (not item)
               (values #f s)
               (let-values ([(more-items s) (match-items s)])
                 (if (not more-items)
                     (values (list item) s)
                     (values (cons item more-items) s)))))))

  (define match-variants (multiple-matcher match-variant))
  (define match-extensions (multiple-matcher match-extension))

  (define (match-langtag s)
    (let-values ([(language s) (match-language s)])
      (if (not language)
          #f
          (let-values ([(script s) (match-script s)])
            (let-values ([(region s) (match-region s)])
              (let-values ([(variants s) (match-variants s)])
                (let-values ([(extensions s) (match-extensions s)])
                  (let-values ([(privateuse s) (match-privateuse-subtags s)])
                    (if (string=? s "")
                        (list language script region variants extensions
                              privateuse)
                        #f)))))))))

  (define match-grandfathered
    (let* ([re-str
            (string-append
             "(en[-_]gb[-_]oed|"
             "i[-_](ami|bnn|default|enochian|hak|klingon|lux|mingo|navajo|pwn|tao|tay|tsu)|"
             "sgn[-_](be[-_](fr|nl)|ch[-_]de)|"
             "art[-_]lojban|cel[-_]gaulish|"
             "no[-_](bok|nyn)|"
             "zh[-_](guoyu|hakka|min|min[-_]nan|xiang))\\z")]
           [re (rexp:compile-study re-str)])
      (lambda (s)
        (let* ([s^ (string-locale-downcase s posix-locale)]
               [m (rexp:match re s^)])
          (if (not m) #f s)))))

  (define match-privateuse (simple-matcher privateuse-str 0))

  (define (parse-ietf-language-tag tag)
    (let ([t (match-grandfathered tag)])
      (if t
          (cons 'grandfathered t)
          (let ([t (match-privateuse tag)])
            (if t
                (cons 'privateuse t)
                (let ([t (match-langtag tag)])
                  (if t
                      (cons 'langtag t)
                      #f)))))))

  #|
  (write (parse-ietf-language-tag "sl-abc-def-Cyrl-YU-rozaj-solba-1994-b-1234-a-Foobar-x-b-1234-a-Foobar"))
  (newline)

  (write (parse-ietf-language-tag "x-sl-abc-def-Cyrl-YU-rozaj-solba-1994-b-1234-a-Foobar-x-b-1234-a-Foobar"))
  (newline)

  (write (parse-ietf-language-tag "i-enoCHIAN"))
  (newline)

  (write (parse-ietf-language-tag "i-enoCHIANfoo"))
  (newline)
  |#

  (define (ietf-language-tag=? a b)
    (string-locale-ci=? a b posix-locale))

  (define (reformat-ietf-language-tag tag)
    ;; See http://tools.ietf.org/html/rfc5646, section 2.1.1.
    (if (not (string? tag))
        tag
        (let* ([subtags
                (list->vector
                 (map
                  (cut string-map       ; Accept #\_ in place of #\-
                       (lambda (c) (if (char=? c #\_) #\- c)) <>)
                  (map (cut string-locale-downcase <> posix-locale)
                       (string-split tag #\-))))]
               [n (vector-length subtags)])
          (when (<= 2 n)
            (do ([i 1 (+ i 1)]) ([= i n])
              (unless (= 1 (string-length (vector-ref subtags (- i 1))))
                (let ([subtag (vector-ref subtags i)])
                  (case (string-length subtag)
                    [(2) (vector-set! subtags i
                                      (string-locale-upcase
                                       subtag posix-locale))]
                    [(4) (vector-set! subtags i
                                      (string-locale-titlecase
                                       subtag posix-locale))]
                    [else *unspecified*])))))
          (string-join (vector->list subtags) "-"))))

  ) ;; end of library.
