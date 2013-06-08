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
   ;; This comparison takes no account of language ranges, fallbacks,
   ;; etc.  [http://tools.ietf.org/html/rfc4647], nor does it
   ;; canonicalize; the match must be exact except for formatting.
   ietf-language-tag=?

   ;; (reformat-ietf-language-tag tag) → tag-in-recommended-format
   ;;
   ;; Does not check whether the tag is well formed.
   ;;
   ;; See http://tools.ietf.org/html/rfc5646, section 2.1.1.
   reformat-ietf-language-tag

   ;; (parse-ietf-language-tag tag) → decomposition or #f
   ;;
   ;; The tag is first reformatted by reformat-ietf-language-tag. One
   ;; consequence is that equality of two decompositions can be tested
   ;; with ‘equal?’ (as long as you do not need canonicalization).
   ;;
   ;; If the tag is not well formed, then #f is returned. No further
   ;; validation is done.
   ;;
   ;; Examples:
   ;;
   ;;    (parse-ietf-language-tag "sl-abc-def-Cyrl-YU-rozaj-solba-1994-b-1234-2222-a-Foobar-x-b-1234-a-Foobar")
   ;;        → '(langtag "sl-abc-def" "Cyrl" "YU"
   ;;                     ("rozaj" "solba" "1994")
   ;;                     ("b-1234-2222" "a-foobar")
   ;;                     "x-b-1234-a-foobar")
   ;;
   ;;    (parse-ietf-language-tag "en-US") → '(langtag "en" #f "US" #f #f #f)
   ;;
   ;;    (parse-ietf-language-tag "x-sl-abc-def-Cyrl-YU-rozaj-solba-1994-b-1234-a-Foobar-x-b-1234-a-Foobar")
   ;;        → '(privateuse . "x-sl-abc-def-cyrl-yu-rozaj-solba-1994-b-1234-a-foobar-x-b-1234-a-foobar")
   ;;
   ;;    (parse-ietf-language-tag "i-enoCHIAN") → '(grandfathered . "i-enochian")
   ;;
   ;;    (parse-ietf-language-tag "sgn-be-nl") → '(grandfathered . "sgn-BE-NL")
   ;;
   ;;    (parse-ietf-language-tag "not=a=tag") → #f
   ;;
   ;; Underscores are accepted in place of hyphens:
   ;;
   ;;    (parse-ietf-language-tag "en_US") → '(langtag "en" #f "US" #f #f #f)
   ;;
   parse-ietf-language-tag

   ;; (unparse-ietf-language-tag decomposition) → string
   unparse-ietf-language-tag
   )

  (import (sortsmill strings rexp)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) break)
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
  (define letter-x "[xX]")

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

  (define privateuse-str (format #f "(~a(~a~a{1,8})+)\\z"
                                 letter-x separator alphanum))

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
    (let ([tag (reformat-ietf-language-tag tag)])
      (let ([t (match-grandfathered tag)])
        (if t
            (cons 'grandfathered t)
            (let ([t (match-privateuse tag)])
              (if t
                  (cons 'privateuse t)
                  (let ([t (match-langtag tag)])
                    (if t
                        (cons 'langtag t)
                        #f))))))))

  (define (ietf-language-tag=? a b)
    (let ([a-decomposition (parse-ietf-language-tag a)])
      (if a-decomposition
          (let ([b-decomposition (parse-ietf-language-tag b)])
            (if b-decomposition
                (equal? a-decomposition b-decomposition)
                (not-an-ietf-language-tag 'ietf-language-tag=? b)))
          (not-an-ietf-language-tag 'ietf-language-tag=? a))))

  (define (not-an-ietf-language-tag who obj)
    (assertion-violation who (_ "not an IETF language tag") obj))

  (define (reformat-ietf-language-tag tag)
    ;; See http://tools.ietf.org/html/rfc5646, section 2.1.1.
    (if (not (string? tag))
        tag
        (let* ([tag                     ; Accept #\_ in place of #\-
                (string-map (lambda (c) (if (char=? c #\_) #\- c)) tag)]
               [subtags
                (map (cut string-locale-downcase <> posix-locale)
                     (string-split tag #\-))])
          (match subtags
            [() ""]
            [(lang) lang]
            [((? string-length=1? x) . more-subtags)
             (join-subtags (cons x more-subtags))]
            [(lang . more-subtags)
             (let-values
                 ([(pre-singletons post-singletons)
                   (break string-length=1? more-subtags)])
               (join-subtags
                (append (list lang)
                        (map (lambda (subtag)
                               (case (string-length subtag)
                                 [(2) (string-locale-upcase
                                       subtag posix-locale)]
                                 [(4) (string-locale-titlecase
                                       subtag posix-locale)]
                                 [else subtag]))
                             pre-singletons)
                        post-singletons)))] ))))

  (define join-subtags (cut string-join <> "-"))

  (define (string-length=1? s)
    (= 1 (string-length s)))

  (define (unparse-langtag langtag-decomposition)
    (let ([strings (map (lambda (entry)
                          (if (string? entry)
                              entry
                              (unparse-langtag entry)))
                        langtag-decomposition)])
      (string-join strings "-")))

  (define (unparse-ietf-language-tag decomposition)
    (match decomposition
      [('langtag . langtag-decomposition)
       (unparse-langtag langtag-decomposition)]
      [((or 'grandfathered 'privateuse) . tag)
       tag]
      [_ (assertion-violation 'unparse-ietf-language-tag
                              (_ "not an IETF language tag decomposition")
                              decomposition)]))

;;;  (write (parse-ietf-language-tag "sl-abc-def-Cyrl-YU-rozaj-solba-1994-b-1234-2222-a-Foobar-x-b-1234-a-Foobar"))
;;;  (newline)
;;;  (write (ietf-language-tag=?
;;;          "sl-abc-def-Cyrl-YU-rozaj-solba-1994-b-1234-a-Foobar-x-b-1234-a-Foobar"
;;;          "SL-ABC-DEF-CYRL-yu-ROZAJ-SOLBA-1994-B-1234-A-FOOBAR-X-B-1234-A-FOOBAR"))
;;;  (newline)
;;;
;;;  (write (parse-ietf-language-tag "x-sl-abc-def-Cyrl-YU-rozaj-solba-1994-b-1234-a-Foobar-x-b-1234-a-Foobar"))
;;;  (newline)
;;;
;;;  (write (parse-ietf-language-tag "i-enoCHIAN"))
;;;  (newline)
;;;
;;;  (write (parse-ietf-language-tag "i-enoCHIANfoo"))
;;;  (newline)

  ) ;; end of library.
