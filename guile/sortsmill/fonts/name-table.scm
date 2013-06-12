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

(library (sortsmill fonts name-table)

  (export
   name-table-set!
   name-table-remove!
   name-table-ref
   name-table-partition

   name-table:check-language-id

   ;; (name-table:name-id->mnemonic integer) → string or #f
   ;; (name-table:name-mnemonic->id string) → integer or #f
   ;; (name-table:windows-language-id->mnemonic integer) → string or #f
   ;; (name-table:windows-language-mnemonic->id string) → integer or #f
   name-table:name-id->mnemonic
   name-table:name-mnemonic->id
   name-table:windows-language-id->mnemonic
   name-table:windows-language-mnemonic->id

   ;; (name-table:name-ids) → list-of-integers
   ;; (name-table:name-mnemonics) → list-of-strings
   ;; (name-table:windows-languages-ids) → list-of-integers
   ;; (name-table:windows-language-mnemonics) → list-of-strings
   name-table:name-ids
   name-table:name-mnemonics      ; Lists the primary mnemonics, only.
   name-table:windows-language-ids
   name-table:windows-language-mnemonics ; Lists the primary mnemonics, only.

   name-table:read-from-sfnt-at-offset
   )

  (import (sortsmill dynlink)
          (sortsmill fonts opentype-io)
          (sortsmill strings ietf-language-tags)
          (sortsmill postscript)
          (sortsmill kwargs)
          (sortsmill notices)
          (sortsmill i18n)
          (rnrs)
          (except (guile) error)
          (only (srfi :1) take filter-map list-tabulate partition)
          (only (srfi :26) cut)
          (system foreign)
          (ice-9 match)
          (ice-9 format))

  ;;-------------------------------------------------------------------------

  (eval-when (compile load eval)
    (sortsmill-dynlink-load-extension "init_sortsmill_parsettf_guile"))

  ;;-------------------------------------------------------------------------

;;;; FIXME: without-adjacent-duplicates looks very reusable. If made
;;;; reusable one might also, as well, rewrite it in C, which should
;;;; not be difficult. It is basically a fast delete-duplicates for
;;;; long, sorted lists.
  (define (without-adjacent-duplicates dup? lst)
    (if (null? lst)
        '()
        (reverse
         (fold-left
          (lambda (prior x)
            (if (dup? x (car prior))
                prior
                (cons x prior)))
          (take lst 1)
          (cdr lst)))))

  ;;-------------------------------------------------------------------------
  ;;
  ;; How the name table is represented internally.
  ;;
  ;; Currently this storage is based on format 1 of the Naming Table,
  ;; which is specified at
  ;; http://www.microsoft.com/typography/otspec/name.htm
  ;;
  ;; The name table is stored as a Guile vector containing one element:
  ;;
  ;;    #(name-records)
  ;;
  ;; The name-records field is a Guile list of ‘name records’.
  ;;
  ;; A ‘name record’ is a Guile vector like this:
  ;;
  ;;    #(platform-id language-id name-id value)
  ;;
  ;; The platform-id is an unsigned Guile integer.
  ;;
  ;; The language-id is either an unsigned Guile integer less than
  ;; #x8000 or a Guile string. If it is an integer, then it represents a
  ;; Windows platform-dependent language ID
  ;; [http://www.microsoft.com/typography/otspec/name.htm]. If the
  ;; language-id is a string, it represents a case-insensitive (but
  ;; arbitrarily capitalized) BCP 47 language tag
  ;; [http://en.wikipedia.org/wiki/IETF_language_tag].
  ;;
  ;; The name field is an unsigned Guile integer representing a Name ID
  ;; [http://www.microsoft.com/typography/otspec/name.htm].
  ;;
  ;; The value field is a Guile string.
  ;;  
  ;;-------------------------------------------------------------------------

  (define/kwargs (name-table-set! name-table platform-id language-id name-id
                                  value [strict? #f])
    (assert (string? value))
    (assert (and (integer? name-id) (not (negative? name-id))))
    (assert-name-table-platform-supported 'name-table-set! platform-id)
    (let ([language-id (name-table:check-language-id language-id)])
      (if language-id
          (let ([lst-without-old-entry
                 (remp (lambda (entry)
                         (name-record-keys-match?
                          'name-table-set! entry platform-id language-id name-id))
                       (vector-ref name-table 0))]
                [new-entry `#(,platform-id
                              ,(reformat-ietf-language-tag language-id)
                              ,name-id ,value)])
            (vector-set! name-table 0 (cons new-entry lst-without-old-entry)))
          (when strict?
            (not-a-valid-language-id 'name-table-set! language-id)))))

  (define/kwargs (name-table-remove! name-table platform-id language-id
                                     name-id [strict? #f])
    (assert (or (eq? name-id #t) (and (integer? name-id) (not (negative? name-id)))))
    (unless (eq? platform-id #t)
      (assert-name-table-platform-supported 'name-table-remove! platform-id))
    (let ([language-id (name-table:check-language-id language-id #t)])
      (if language-id
          (let ([lst-without-old-entry
                 (remp (lambda (entry)
                         (name-record-keys-match?
                          'name-table-remove! entry platform-id language-id name-id))
                       (vector-ref name-table 0))])
            (vector-set! name-table 0 lst-without-old-entry))
          (when strict?
            (not-a-valid-language-id 'name-table-set! language-id)))))

  (define/kwargs (name-table-ref name-table platform-id language-id name-id
                                 [strict? #f])
    (assert (or (eq? name-id #t) (and (integer? name-id) (not (negative? name-id)))))
    (unless (eq? platform-id #t)
      (assert-name-table-platform-supported 'name-table-ref platform-id))
    (let ([language-id (name-table:check-language-id language-id #t)])
      (if language-id
          (let ([sublist
                 (memp (lambda (entry) (name-record-keys-match?
                                        'name-table-ref entry platform-id language-id
                                        name-id))
                       (vector-ref name-table 0))])
            (if sublist (car sublist) #f))
          (if strict?
              (not-a-valid-language-id 'name-table-set! language-id)
              #f))))

  (define/kwargs (name-table-partition name-table
                                       [platform-id #t]
                                       [language-id #t]
                                       [name-id #t])
    (let ([language-id (name-table:check-language-id language-id #t)])
      (if language-id
          (let-values ([(matched unmatched)
                        (partition
                         (lambda (entry)
                           (name-record-keys-match? 'name-table-ref entry
                                                    platform-id language-id
                                                    name-id))
                         (vector-ref name-table 0))])
            (values `#(,matched) `#(,unmatched)))
          (values #f #f))))

  (define (name-record-keys-match? who name-record platform-id language-id name-id )
    (if (and (or (eq? name-id #t) (eqv? name-id (vector-ref name-record 2)))
             (or (eq? platform-id #t) (eqv? platform-id (vector-ref name-record 0))))
        (if (eq? language-id #t)
            #t
            (cond [(integer? language-id)
                   (eqv? language-id (vector-ref name-record 1))]
                  [(string? language-id)
                   (ietf-language-tag=? language-id (vector-ref name-record 1))]
                  [else
                   (assertion-violation who (_ "expected an integer or string")
                                        language-id)]))
        #f))

  (define/kwargs (name-table:check-language-id language-id [allow-wildcard? #f])
    (if (and allow-wildcard? (eq? language-id #t))
        #t
        (cond [(integer? language-id)     ; Platform-specific language ID.
               (if (<= 0 language-id #x7fff) language-id #f)]
              [(string? language-id)        ; An IETF language tag.
               (let ([decomposition (parse-ietf-language-tag language-id)])
                 (if decomposition (unparse-ietf-language-tag decomposition) #f))]
              [else #f])))

  (define (assert-name-table-platform-supported who platform-id)
    ;; Platforms that may appear in a ‘name’ table. (There are more
    ;; that may appear in a ‘cmap’ table.)
    (unless (or (eqv? platform-id 0)       ; Unicode.
                (eqv? platform-id 1)       ; Macintosh.
                (eqv? platform-id 3))      ; Windows.
      (assertion-violation who (_ "platform ID not supported in name tables")
                           platform-id)))

  (define (not-a-valid-language-id who language)
    (assertion-violation who (_ "not a value language ID") language))

  ;;-------------------------------------------------------------------------

  (define name-id-associations
    '((2 . "Subfamily") ;; ← The primary mnemonic.
      (2 . "SubFamily") ;; ← The mnemonic used in FontForge.
      (2 . "Style")
      (2 . "Styles")
      (0 . "Copyright")
      (1 . "Family")
      (4 . "Fullname")
      (3 . "UniqueID")
      (5 . "Version")
      (6 . "PostScript Name") ;; ← The primary mnemonic.
      (6 . "PostScriptName")  ;; ← The mnemonic used in FontForge.
      (7 . "Trademark")
      (8 . "Manufacturer")
      (9 . "Designer")
      (10 . "Descriptor")
      (11 . "Vendor URL")
      (12 . "Designer URL")
      (13 . "License")
      (14 . "License URL")
      ;; Slot 15 is reserved.
      (16 . "Preferred Family")
      (17 . "Preferred Subfamily") ;; ← The primary mnemonic.
      (17 . "Preferred Styles") ;; ← The mnemonic used in FontForge.
      (17 . "Preferred SubFamily")
      (17 . "Preferred Style")
      (18 . "Compatible Full")
      (19 . "Sample Text")
      (20 . "CID findfont Name")
      (21 . "WWS Family")
      (22 . "WWS Subfamily") ;; ← The primary mnemonic and also the mnemonic used in FontForge.
      (22 . "WWS SubFamily")
      (22 . "WWS Style")
      (22 . "WWS Styles")))

  (define windows-language-associations
    '((#x436 . "Afrikaans")
      (#x41c . "Albanian")
      (#x45e . "Amharic")
      (#x401 . "Arabic (Saudi Arabia)")
      (#x801 . "Arabic (Iraq)")
      (#xc01 . "Arabic (Egypt)")
      (#x1001 . "Arabic (Libya)")
      (#x1401 . "Arabic (Algeria)")
      (#x1801 . "Arabic (Morocco)")
      (#x1C01 . "Arabic (Tunisia)")
      (#x2001 . "Arabic (Oman)")
      (#x2401 . "Arabic (Yemen)")
      (#x2801 . "Arabic (Syria)")
      (#x2c01 . "Arabic (Jordan)")
      (#x3001 . "Arabic (Lebanon)")
      (#x3401 . "Arabic (Kuwait)")
      (#x3801 . "Arabic (U.A.E.)")
      (#x3c01 . "Arabic (Bahrain)")
      (#x4001 . "Arabic (Qatar)")
      (#x42b . "Armenian")
      (#x44d . "Assamese")
      (#x42c . "Azeri (Latin)")
      (#x82c . "Azeri (Cyrillic)")
      (#x42d . "Basque")
      (#x423 . "Byelorussian")
      (#x445 . "Bengali")
      (#x845 . "Bengali Bangladesh")
      (#x402 . "Bulgarian")
      (#x455 . "Burmese")
      (#x403 . "Catalan")
      (#x453 . "Cambodian")
      (#x45c . "Cherokee")
      (#x404 . "Chinese (Taiwan)")
      (#x804 . "Chinese (PRC)")
      (#xc04 . "Chinese (Hong Kong)")
      (#x1004 . "Chinese (Singapore)")
      (#x1404 . "Chinese (Macau)")
      (#x41a . "Croatian")
      (#x101a . "Croatian Bosnia/Herzegovina")
      (#x405 . "Czech")
      (#x406 . "Danish")
      (#x465 . "Divehi")
      (#x413 . "Dutch")
      (#x813 . "Flemish (Belgian Dutch)")
      (#x466 . "Edo")
      (#x809 . "English (British)")
      (#x409 . "English (US)")
      (#x1009 . "English (Canada)")
      (#xc09 . "English (Australian)")
      (#x1409 . "English (New Zealand)")
      (#x1809 . "English (Irish)")
      (#x1c09 . "English (South Africa)")
      (#x2009 . "English (Jamaica)")
      (#x2409 . "English (Caribbean)")
      (#x2809 . "English (Belize)")
      (#x2c09 . "English (Trinidad)")
      (#x3009 . "English (Zimbabwe)")
      (#x3409 . "English (Philippines)")
      (#x3809 . "English (Indonesia)")
      (#x3c09 . "English (Hong Kong)")
      (#x4009 . "English (India)")
      (#x4409 . "English (Malaysia)")
      (#x425 . "Estonian")
      (#x438 . "Faeroese")
      (#x429 . "Farsi")
      (#x464 . "Filipino")
      (#x40b . "Finnish")
      (#x40c . "French French")
      (#x80c . "French Belgium")
      (#xc0c . "French Canadian")
      (#x100c . "French Swiss")
      (#x140c . "French Luxembourg")
      (#x180c . "French Monaco")
      (#x1c0c . "French West Indies")
      (#x200c . "French Réunion")
      (#x240c . "French D.R. Congo")
      (#x280c . "French Senegal")
      (#x2c0c . "French Camaroon")
      (#x300c . "French Côte d'Ivoire")
      (#x340c . "French Mali")
      (#x380c . "French Morocco")
      (#x3c0c . "French Haiti")
      (#xe40c . "French North Africa")
      (#x462 . "Frisian")
      (#x467 . "Fulfulde")
      (#x43c . "Gaelic (Scottish)")
      (#x83c . "Gaelic (Irish)")
      (#x467 . "Galician")
      (#x437 . "Georgian")
      (#x407 . "German German")
      (#x807 . "German Swiss")
      (#xc07 . "German Austrian")
      (#x1007 . "German Luxembourg")
      (#x1407 . "German Liechtenstein")
      (#x408 . "Greek")
      (#x474 . "Guarani")
      (#x447 . "Gujarati")
      (#x468 . "Hausa")
      (#x475 . "Hawaiian")
      (#x40d . "Hebrew")
      (#x439 . "Hindi")
      (#x40e . "Hungarian")
      (#x469 . "Ibibio")
      (#x40f . "Icelandic")
      (#x470 . "Igbo")
      (#x421 . "Indonesian")
      (#x45d . "Inuktitut")
      (#x410 . "Italian")
      (#x810 . "Italian Swiss")
      (#x411 . "Japanese")
      (#x44b . "Kannada")
      (#x471 . "Kanuri")
      (#x860 . "Kashmiri (India)")
      (#x43f . "Kazakh")
      (#x453 . "Khmer")
      (#x440 . "Kirghiz")
      (#x457 . "Konkani")
      (#x412 . "Korean")
      (#x812 . "Korean (Johab)")
      (#x454 . "Lao")
      (#x426 . "Latvian")
      (#x476 . "Latin")
      (#x427 . "Lithuanian")
      (#x827 . "Lithuanian (Classic)")
      (#x42f . "Macedonian")
      (#x43e . "Malay")
      (#x83e . "Malay (Brunei)")
      (#x44c . "Malayalam")
      (#x43a . "Maltese")
      (#x458 . "Manipuri")
      (#x481 . "Maori")
      (#x44e . "Marathi")
      (#x450 . "Mongolian (Cyrillic)")
      (#x850 . "Mongolian (Mongolian)")
      (#x461 . "Nepali")
      (#x861 . "Nepali (India)")
      (#x414 . "Norwegian (Bokmal)")
      (#x814 . "Norwegian (Nynorsk)")
      (#x448 . "Oriya")
      (#x472 . "Oromo")
      (#x479 . "Papiamentu")
      (#x463 . "Pashto")
      (#x415 . "Polish")
      (#x416 . "Portuguese (Portugal)")
      (#x816 . "Portuguese (Brasil)")
      (#x446 . "Punjabi (India)")
      (#x846 . "Punjabi (Pakistan)")
      (#x46b . "Quecha (Bolivia)")
      (#x86b . "Quecha (Ecuador)")
      (#xc6b . "Quecha (Peru)")
      (#x417 . "Rhaeto-Romanic")
      (#x418 . "Romanian")
      (#x818 . "Romanian (Moldova)")
      (#x419 . "Russian")
      (#x819 . "Russian (Moldova)")
      (#x43b . "Sami (Lappish)")
      (#x43b . "Sanskrit")
      (#x46c . "Sepedi")
      (#xc1a . "Serbian (Cyrillic)")
      (#x81a . "Serbian (Latin)")
      (#x459 . "Sindhi India")
      (#x859 . "Sindhi Pakistan")
      (#x45b . "Sinhalese")
      (#x41b . "Slovak")
      (#x424 . "Slovenian")
      (#x42e . "Sorbian")
      (#x40a . "Spanish (Traditional)")
      (#x80a . "Spanish Mexico")
      (#xc0a . "Spanish (Modern)")
      (#x100a . "Spanish (Guatemala)")
      (#x140a . "Spanish (Costa Rica)")
      (#x180a . "Spanish (Panama)")
      (#x1c0a . "Spanish (Dominican Republic)")
      (#x200a . "Spanish (Venezuela)")
      (#x240a . "Spanish (Colombia)")
      (#x280a . "Spanish (Peru)")
      (#x2c0a . "Spanish (Argentina)")
      (#x300a . "Spanish (Ecuador)")
      (#x340a . "Spanish (Chile)")
      (#x380a . "Spanish (Uruguay)")
      (#x3c0a . "Spanish (Paraguay)")
      (#x400a . "Spanish (Bolivia)")
      (#x440a . "Spanish (El Salvador)")
      (#x480a . "Spanish (Honduras)")
      (#x4c0a . "Spanish (Nicaragua)")
      (#x500a . "Spanish (Puerto Rico)")
      (#x540a . "Spanish (United States)")
      (#xe40a . "Spanish (Latin America)")
      (#x430 . "Sutu")
      (#x441 . "Swahili (Kenyan)")
      (#x41d . "Swedish (Sweden)")
      (#x81d . "Swedish (Finland)")
      (#x45a . "Syriac")
      (#x464 . "Tagalog")
      (#x428 . "Tajik")
      (#x45f . "Tamazight (Arabic)")
      (#x85f . "Tamazight (Latin)")
      (#x449 . "Tamil")
      (#x444 . "Tatar (Tatarstan)")
      (#x44a . "Telugu")
      (#x41e . "Thai")
      (#x451 . "Tibetan (PRC)")
      (#x851 . "Tibetan Bhutan")
      (#x473 . "Tigrinya Ethiopia")
      (#x873 . "Tigrinyan Eritrea")
      (#x431 . "Tsonga")
      (#x432 . "Tswana")
      (#x41f . "Turkish")
      (#x442 . "Turkmen")
      (#x480 . "Uighur")
      (#x422 . "Ukrainian")
      (#x420 . "Urdu (Pakistan)")
      (#x820 . "Urdu (India)")
      (#x443 . "Uzbek (Latin)")
      (#x843 . "Uzbek (Cyrillic)")
      (#x433 . "Venda")
      (#x42a . "Vietnamese")
      (#x452 . "Welsh")
      (#x434 . "Xhosa")
      (#x478 . "Yi")
      (#x43d . "Yiddish")
      (#x46a . "Yoruba")
      (#x435 . "Zulu")))

  (define (name-table:name-id->mnemonic id)
    (assv-ref name-id-associations id))

  (define (name-table:name-mnemonic->id mnemonic)
    (let ([reversed-alist (map
                           (lambda (pair) (cons (cdr pair) (car pair)))
                           name-id-associations)])
      (assoc-ref reversed-alist mnemonic)))

  (define (name-table:windows-language-id->mnemonic id)
    (assv-ref windows-language-associations id))

  (define (name-table:windows-language-mnemonic->id mnemonic)
    (let ([reversed-alist (map
                           (lambda (pair) (cons (cdr pair) (car pair)))
                           windows-language-associations)])
      (assoc-ref reversed-alist mnemonic)))

  (define name-table:name-ids
    (let ([ids (without-adjacent-duplicates
                = (list-sort < (map car name-id-associations)))])
      (lambda () ids)))

  (define name-table:names-mnemonics
    (let ([langs (map
                  (lambda (id) (name-table:name-id->mnemonic id))
                  (name-table:name-ids))])
      (lambda () langs)))

  (define name-table:windows-language-ids
    (let ([ids (without-adjacent-duplicates
                = (list-sort < (map car windows-language-associations)))])
      (lambda () ids)))

  (define name-table:windows-language-mnemonics
    (let* ([pairs-without-dups (without-adjacent-duplicates
                                (lambda (a b) (= (car a) (car b)))
                                windows-language-associations)]
           [langs (map cdr pairs-without-dups)])
      (lambda () langs)))

  ;;-------------------------------------------------------------------------

  (define (name-table:read-from-sfnt-at-offset port/fd offset)
    (let ([port (if (port? port/fd) port/fd (fdes->inport port/fd))])
      (ot:at-port-position
       port offset
       (lambda ()
         (let*-values
             ([(table-format) (ot:read-USHORT port)]
              [(namerec-count) (ot:read-USHORT port)]
              [(string-offset) (ot:read-USHORT port)]
              [(namerecs)
               (list-tabulate namerec-count
                              (lambda (i)
                                (read-name-record
                                 port (+ offset string-offset))))]
              [(langtag-count) (if (= 1 table-format) (ot:read-USHORT port) 0)]
              [(langtagrecs)
               (list-tabulate langtag-count
                              (lambda (i)
                                (read-lang-tag-record
                                 port (+ offset string-offset))))]
              [(namerecs) (fix-up-namerecs namerecs langtagrecs)]
              [(namerecs bad-ps-name) (fix-postscript-font-name namerecs)]
              [(name-table) `#(,namerecs)])
           #|
           (format #t "table-format = ~a\n" table-format)
           (format #t "namerec-count = ~a\n" namerec-count)
           (format #t "string-offset = ~a\n" string-offset)
           (format #t "e = ~a\n" (name-table-ref name-table 3 #t #t))
           ;;(format #t "e = ~a\n" (name-table-remove! name-table 3 #t #t))
           (format #t "e = ~a\n" (name-table-set! name-table 3 1033 14 "set!"))
           (format #t "e = ~a\n" (name-table-ref name-table 3 #t #t))
           (format #t "e = ~a\n" (name-table-remove! name-table 3 #t #t))
           (format #t "e = ~a\n" (name-table-ref name-table 3 #t #t))
           |#
           (format #t "name-table = ~a\n" name-table)
           ;;(let-values ([(a b) (name-table-partition name-table
           ;;                                          #:platform-id 1)])
           ;;  (format #t "partitioned name-table a = ~a\n" a)
           ;;  (format #t "partitioned name-table b = ~a\n" b)
           ;;  )
           )))))

  (define (read-name-record port string-buffer-position)
    (let* ([platform-id (ot:read-USHORT port)]
           [encoding-id (ot:read-USHORT port)]
           [language-id (ot:read-USHORT port)]
           [name-id (ot:read-USHORT port)]
           [length (ot:read-USHORT port)]
           [offset (ot:read-USHORT port)]
           [str (read-ot-string length
                                port (+ string-buffer-position offset)
                                platform-id encoding-id language-id)])
      (if str
          `#(,platform-id ,language-id ,name-id ,str)
          #f)))

  (define (read-lang-tag-record port string-buffer-position)
    (let* ([length (ot:read-USHORT port)]
           [offset (ot:read-USHORT port)]
           [bv (ot:read-string length port (+ string-buffer-position offset))]
           [str (utf16->string bv (endianness big))]
           [decomposition (parse-ietf-language-tag str)]
           [tag (if decomposition (unparse-ietf-language-tag decomposition) #f)])
      tag))

  (define (fix-up-namerecs namerecs langtagrecs)
    (let* ([tags (list->vector langtagrecs)]
           [n (vector-length tags)])
      (filter-map
       (lambda (nrec)
         (let ([language-id (vector-ref nrec 1)])
           (cond
            [(< language-id #x8000) nrec] ; Platform-specific language ID.
            [(< (- language-id #x8000) n) ; IETF language tag.
             (let* ([i (- language-id #x8000)]
                    [tag (vector-ref tags i)])
               (cond
                [tag `#(,(vector-ref nrec 0)
                        ,tag
                        ,(vector-ref nrec 2)
                        ,(vector-ref nrec 3))]
                [else
                 ;; Ignore any name record whose language tag is
                 ;; unparseable.
                 #f]))]
            [else
             ;; Ignore any name record whose platform-specific
             ;; language ID is greater than #x7FFF.
             #f])))
       namerecs)))

  (define (fix-postscript-font-name namerecs)

    (define (warn-bad-name value errors platform-id)
      (post-fontforge-error
       (_ "Bad Font Name")
       (format #f
               (_ "The PostScript font name \"~a\"\nfor platform ~a is invalid.\nThe PostScript name should be printable ASCII,\nmust not contain (){}[]<>%%/, space,\nor a byte order mark,\nmust be no longer than 63 characters,\nand cannot be a PostScript number.")
               value platform-id)))

    (define (warn-bad-language platform-id)
      (post-fontforge-error
       (_ "Bad Font Name")
       (format #f
               (_ "The PostScript font name (Naming Table entry 6)\nfor platform ~a is assigned a language ID\nother than the platform-specific code for US English.")
               platform-id)))

    (define (warn-bad-platform platform-id)
      (post-fontforge-error
       (_ "Bad Font Name")
       (format #f
               (_ "Their is a PostScript font name entry\n(Naming Table entry 6) for platform ~a,\nwhich is not allowed.")
               platform-id)))

    (let* ([bad-ps-name #f]
           [new-namerecs
            (filter-map
             (lambda (nrec)
               (match nrec
                 [#((? (lambda (p) (= p 1)) platform-id)
                    language-id
                    (? (lambda (n) (eqv? 6 n)) name-id)
                    value)
                  ;; PostScript name for Macintosh platform.
                  (unless (zero? language-id)
                    (set! bad-ps-name #t)
                    (warn-bad-language platform-id))
                  (let-values ([(ps-name errors)
                                (validate-postscript-font-name value)])
                    (unless (null? errors)
                      (set! bad-ps-name #t)
                      (warn-bad-name value errors platform-id))
                    `#(,platform-id 0 ,name-id ,ps-name))]

                 [#((? (lambda (p) (= p 3)) platform-id)
                    language-id
                    (? (lambda (n) (eqv? 6 n)) name-id)
                    value)
                  ;; PostScript name for Windows platform.
                  (unless (= #x409 language-id)
                    (set! bad-ps-name #t)
                    (warn-bad-language platform-id))
                  (let-values ([(ps-name errors)
                                (validate-postscript-font-name value)])
                    (unless (null? errors)
                      (set! bad-ps-name #t)
                      (warn-bad-name value errors platform-id))
                    `#(,platform-id #x409 ,name-id ,ps-name))]

                 [#(platform-id _ (? (lambda (n) (eqv? 6 n)) name-id) _)
                  ;; PostScript name for other platforms. These are not
                  ;; allowed by OpenType, so ignore the name record.
                  (set! bad-ps-name #t)
                  (warn-bad-platform platform-id)
                  #f]

                 [nrec nrec]))
             namerecs)])
      (values namerecs bad-ps-name)))

  (define (read-ot-string length port offset platform
                          platform-specific-encoding language)
    (let ([bv (ot:read-string length port offset)])
      (recode-ot-string-as-utf8 (bytevector->pointer bv)
                                (bytevector-length bv)
                                platform platform-specific-encoding
                                language)))

  ;;-------------------------------------------------------------------------

  (define* (validate-postscript-font-name font-name)
    (let*-values
        ([(name errors) (values font-name '())]
         [(name errors) (check-for-byte-order-mark name errors)]
         [(name errors) (check-for-postscript-number name errors)]
         [(name errors) (check-for-illegal-characters name errors)]
         [(name errors) (check-for-font-name-too-long name errors)])
      (values name errors)))

  (define (check-for-byte-order-mark font-name errors)
    (if (and (positive? (string-length font-name))
             (char=? #\xFEFF (string-ref font-name 0)))
        (values (substring font-name 1) (cons 'byte-order-mark errors))
        (values font-name errors)))

  (define (check-for-postscript-number font-name errors)
    (if (postscript-number? font-name)
        (values (string-append "font-" font-name)
                (cons 'postscript-number errors))
        (values font-name errors)))

  (define char-set:postscript-name-legal-chars
    (char-set-delete (char-set-intersection char-set:ascii char-set:graphic)
                     #\( #\[ #\{ #\< #\) #\] #\} #\> #\% #\/))

  (define (check-for-illegal-characters font-name errors)
    (let ([mapped-name (string-map
                        (lambda (c)
                          (if (char-set-contains?
                               char-set:postscript-name-legal-chars c)
                              c
                              #\_))
                        font-name)])
      (values mapped-name (if (string=? font-name mapped-name)
                              errors
                              (cons 'illegal-characters errors)))))

  (define (check-for-font-name-too-long font-name errors)
    (if (< 63 (string-length font-name))
        (values (substring font-name 0 63) (cons 'too-long errors))
        (values font-name errors)))

  ;;-------------------------------------------------------------------------

  ) ;; end of library.
